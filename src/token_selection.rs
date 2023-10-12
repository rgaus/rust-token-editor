use std::collections::HashMap;

use crate::token::*;
use crate::token_match_template::*;
use crate::engine::*;
use crate::text_utils::*;

// A SequentialTokenSelection represents a range of characters within a document.
//
// It's anchored at a token at a given offset, and then can either go forwards or backwards by a
// given number of characters. This structure is used to represent any sort of region in the
// document - it's what is returned by document.read, document.read_forwards_until, etc
#[derive(Debug, Clone, PartialEq)]
pub struct SequentialTokenSelection {
    pub starting_token_id: uuid::Uuid,
    pub starting_token_offset: usize,
    pub is_backwards: bool,
    pub char_count: usize,
}

impl SequentialTokenSelection {
    pub fn new(
        starting_token_id: uuid::Uuid,
        starting_token_offset: usize,
        char_count: usize
    ) -> Self {
        Self {
            starting_token_id,
            starting_token_offset,
            is_backwards: false,
            char_count,
        }
    }
    pub fn new_backwards(
        starting_token_id: uuid::Uuid,
        starting_token_offset: usize,
        char_count: usize
    ) -> Self {
        Self {
            starting_token_id,
            starting_token_offset,
            is_backwards: true,
            char_count,
        }
    }

    // Creates a new SequentialTokenSelection by taking two absolute offsets in the token stream
    pub fn new_from_offsets(
        document: &mut Document,
        offset_a: usize,
        offset_b: usize,
    ) -> Result<Self, String> {
        let tokens_collection = document.tokens_mut();

        let (start_offset, end_offset) = if offset_a <= offset_b {
            (offset_a, offset_b)
        } else {
            (offset_b, offset_a)
        };

        let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(start_offset) else {
            return Err(format!("Cannot get start token at offset {} in tokens collection!", start_offset));
        };

        Ok(Self::new(
            start_token.id,
            start_token_offset,
            end_offset - start_offset,
        ))
    }

    // Remove all tokens (and any of their children!) within the token range.
    //
    // If `keep_first_token` is true, then the first token is kept in the token list but left
    // blank, likely because the user execured a change rather than a delete and this initial token
    // is where the change text will end up
    pub fn remove_deep(
        &self,
        document: &mut Document,
        keep_first_token: bool,
    ) -> Result<Self, String> {
        let forwards_range = self.as_forwards_range(document)?;

        let mut chars_removed = 0;
        let mut is_first = true;
        let mut pointer_id = forwards_range.starting_token_id;
        let mut token_ids_to_remove = vec![];
        let tokens_collection = document.tokens_mut();

        loop {
            let result = {
                let Some(pointer) = tokens_collection.get_by_id(pointer_id) else {
                    return Err(format!("Unable to find token with id {} ({} chars in to removal)", pointer_id, chars_removed));
                };

                Ok((pointer.next_id, pointer.literal.clone()))
            };

            match result {
                Ok((pointer_next_id, pointer_literal)) => {
                    if let Some(literal_text) = pointer_literal {
                        let literal_text_length = literal_text.len();
                        chars_removed += literal_text_length;

                        if chars_removed <= forwards_range.char_count {
                            if is_first {
                                if forwards_range.starting_token_offset > 0 {
                                    // Keep the first `self.starting_token_offset` chars of this token
                                    tokens_collection.get_by_id_mut(pointer_id, |pointer| {
                                        pointer.literal = Some(String::from(
                                            &literal_text[..self.starting_token_offset]
                                        ));
                                    });
                                } else if keep_first_token {
                                    tokens_collection.get_by_id_mut(pointer_id, |pointer| {
                                        pointer.literal = Some(String::from(""));
                                    });
                                } else {
                                    // Remove this token, it's the first token, but the match starts at the
                                    // very start so it can all go
                                    token_ids_to_remove.push(pointer_id);
                                }
                            } else {
                                // Remove this token, its bounds are fully within `chars_removed`
                                token_ids_to_remove.push(pointer_id);
                            }
                            if chars_removed == forwards_range.char_count {
                                break;
                            }
                        } else {
                            chars_removed -= literal_text_length;
                            // Part of this token needs to stay around - also, this is the last token
                            tokens_collection.get_by_id_mut(pointer_id, |pointer| {
                                if is_first {
                                    // This token is both the first and last token - so,
                                    // this operation only effects this single token
                                    let result_literal = format!(
                                        "{}{}",
                                        &literal_text[..forwards_range.starting_token_offset],
                                        &literal_text[forwards_range.starting_token_offset+forwards_range.char_count..],
                                    );
                                    pointer.literal = Some(String::from(result_literal));
                                } else {
                                    let number_of_chars_to_keep = forwards_range.char_count - chars_removed;
                                    println!("KEEP: {}", number_of_chars_to_keep);
                                    pointer.literal = Some(String::from(&literal_text[number_of_chars_to_keep..]));
                                }
                            });
                            break;
                        }
                    };

                    if let Some(next_id) = pointer_next_id {
                        pointer_id = next_id;
                    } else {
                        // Reached the end of the token stream
                        return Err(format!("Unable to remove SequentialTokenSelection: ran out of tokens (start at {} offset {} and went {} of {} chars)", forwards_range.starting_token_id, forwards_range.starting_token_offset, chars_removed, forwards_range.char_count));
                    }
                    is_first = false;
                },
                Err(err) => {
                    return Err(err)
                },
            }
        }

        for token_id in token_ids_to_remove {
            tokens_collection.remove_deep(token_id)?;
        }

        // Clear out the char count now that all tokens inside have been removed
        let mut new_range = forwards_range.clone();
        new_range.char_count = 0;
        Ok(new_range)
    }

    // Prepends the given token range's text contents with the passed `new_text` value.
    //
    // On its own, this implements an insert at the start of the token range.
    // It can also be used AFTER calling `remove_deep(.., false)` to implement a change.
    pub fn prepend_text(
        &self,
        document: &mut Document,
        new_text: String,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<Option<uuid::Uuid>, String> {
        let tokens_collection = document.tokens_mut();

        let mut complete_new_text = new_text;
        if let Some(starting_token) = tokens_collection.get_by_id(self.starting_token_id) {
            if let Some(literal_text) = &starting_token.literal {
                complete_new_text = String::from(format!(
                    "{}{}",
                    &literal_text[..self.starting_token_offset],
                    complete_new_text, /* this is equal to `new_text` here */
                ));
                // println!("AFTER: '{}' {} > {}+{}", literal_text, literal_text.len(), self.starting_token_offset, self.char_count);
                if literal_text.len() > self.starting_token_offset + self.char_count {
                    complete_new_text = String::from(format!(
                        "{}{}",
                        complete_new_text,
                        &literal_text[self.starting_token_offset..],
                    ));
                }
            }
        };

        // Then change the first one to have the new token text:
        tokens_collection.change_token_literal_text(
            self.starting_token_id,
            complete_new_text,
            token_match_templates_map,
        )
    }

    // When called, converts the given SequentialTokenSelection into forwards form if it is a backwards
    // range. If the range is already forwards, then this is a no-op.
    pub fn as_forwards_range(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        if !self.is_backwards {
            return Ok(self.clone());
        }

        let tokens_collection = document.tokens_mut();

        let mut start_offset = tokens_collection.compute_offset(self.starting_token_id);
        start_offset += self.starting_token_offset;
        start_offset += 1; // Add one because when going backwards, the "side" of the cursor is different

        let end_offset = if self.char_count > start_offset {
            // This range seems to go to before the start of the document?
            0
        } else {
            start_offset - self.char_count
        };

        let Some((token, token_offset)) = tokens_collection.get_by_offset(end_offset) else {
            return Err(format!("Cannot get token at offset {} in tokens collection!", end_offset));
        };

        Ok(Self::new(
            token.id,
            token_offset,
            start_offset - end_offset,
        ))
    }

    // Given a second SequentialTokenSelection, returns a copy of `self` expanded to fit the new
    // specified range. If the ranges do not form a continuous range, the gap between the ranges is
    // added to the resulting range.
    pub fn extend(
        &self,
        document: &mut Document,
        range: SequentialTokenSelection,
    ) -> Result<Self, String> {
        let tokens_collection = document.tokens_mut();

        let mut existing_start_offset = tokens_collection.compute_offset(self.starting_token_id);
        existing_start_offset += self.starting_token_offset;
        let existing_end_offset = if self.is_backwards {
            existing_start_offset - self.char_count
        } else {
            existing_start_offset + self.char_count
        };

        let mut new_start_offset = tokens_collection.compute_offset(range.starting_token_id);
        new_start_offset += range.starting_token_offset;
        let new_end_offset = if range.is_backwards {
            new_start_offset - range.char_count
        } else {
            new_start_offset + range.char_count
        };

        let offsets = vec![
            existing_start_offset,
            existing_end_offset,
            new_start_offset,
            new_end_offset,
        ];
        println!("OFFSETS: {:?}", offsets);
        let Some(smallest_offset) = offsets.iter().min() else {
            return Err(format!("Cannot get smallest offset in vec: {:?}", offsets));
        };
        let Some(largest_offset) = offsets.iter().max() else {
            return Err(format!("Cannot get smallest offset in vec: {:?}", offsets));
        };
        let char_count = largest_offset - smallest_offset;
        println!("CHAR COUNT? {} - {} = {}", largest_offset, smallest_offset, char_count);

        // NOTE: the direction of the resulting range is based off `range` so that if ranges of
        // different directions are put in, the most recent range is the one that dictates
        // direction
        if range.is_backwards {
            let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(*largest_offset) else {
                return Err(format!("Cannot get token at offset {} in tokens collection!", largest_offset));
            };
            println!("EXTEND RESULT: is_backwards=true char_count={char_count}");
            Ok(Self::new_backwards(start_token.id, start_token_offset, char_count))
        } else {
            let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(*smallest_offset) else {
                return Err(format!("Cannot get token at offset {} in tokens collection!", smallest_offset));
            };
            println!("EXTEND RESULT: is_backwards=false char_count={char_count}");
            Ok(Self::new(start_token.id, start_token_offset, char_count))
        }
    }

    // When called, scans forward in the token stream looking for whitespace, and returns a new
    // SequentialTokenSelection with the additional whitespace added to the end
    pub fn select_whitespace_after(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let range = self.as_forwards_range(document)?;
        let tokens_collection = document.tokens_mut();

        // Start seeking from the end of the token forwards, looking for whitespace
        let mut end_offset = tokens_collection.compute_offset(range.starting_token_id);
        end_offset += range.starting_token_offset;
        end_offset += range.char_count;
        document.seek_push(end_offset);

        let result = match document.read_forwards_until(|c, _| !is_whitespace_char(c), false, false) {
            Ok(Some((_, _, range))) => self.extend(document, range),
            // If the range cannot be extended (maybe we're at the end of the file?) then
            // just keep it as it is.
            Ok(None) => Ok(self.clone()),
            Err(e) => Err(e),
        };

        document.seek_pop();
        result
    }

    pub fn text(&self, document: &mut Document) -> String {
        document.tokens().stringify_for_selection(
            self.starting_token_id,
            self.starting_token_offset,
            self.char_count
        )
    }

    pub fn range(&self, document: &mut Document) -> std::ops::Range<usize> {
        let mut start_offset = document.tokens_mut().compute_offset(self.starting_token_id);
        start_offset += self.starting_token_offset;
        let end_offset = start_offset + self.char_count;
        start_offset..end_offset
    }

    pub fn add_to_end(&self, char_count: usize) -> Self {
        let mut copy = self.clone();
        copy.char_count += char_count;
        copy
    }

    // When called, scans bcakwards in the token range, removing all whitespace characters at the
    // end of the SequentialTokenSelection, returning a new range with these changes.
    //
    // This is used in the `change` operation to preserve whitespace after a selection
    pub fn unselect_whitespace_after(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let range = self.as_forwards_range(document)?;
        let tokens_collection = document.tokens_mut();

        // Start seeking from the end of the token forwards, looking for whitespace
        let mut end_offset = tokens_collection.compute_offset(range.starting_token_id);
        end_offset += range.starting_token_offset;
        end_offset += range.char_count;
        document.seek_push(end_offset);

        let result = match document.read_backwards_until(
            |c, _| !is_whitespace_char(c),
            false,
            false,
        ) {
            Ok(Some((_, matched_chars, _))) => {
                println!("MATCHED CHARS: {}", matched_chars);
                let mut new_sequence = range.clone();
                new_sequence.char_count -= matched_chars.len();
                Ok(new_sequence)
            },
            // If the range cannot be extended (maybe we're at the end of the file?) then
            // just keep it as it is.
            Ok(None) => Ok(self.clone()),
            Err(e) => Err(e),
        };

        document.seek_pop();
        result
    }
}