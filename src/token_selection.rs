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

        let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(offset_a) else {
            return Err(format!("Error in SequentialTokenSelection.new_from_offsets: Cannot get start token at offset {} in tokens collection!", offset_a));
        };

        if offset_a <= offset_b {
            Ok(Self::new(
                start_token.id,
                start_token_offset,
                offset_b - offset_a,
            ))
        } else {
            Ok(Self::new_backwards(
                start_token.id,
                start_token_offset,
                offset_a - offset_b,
            ))
        }
    }

    // Creates a new SequentialTokenSelection by taking two absolute offsets in the token stream
    pub fn new_zero_length_at_offset(
        document: &mut Document,
        offset: usize,
    ) -> Result<Self, String> {
        Self::new_from_offsets(document, offset, offset)
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
        let forwards_range = self.as_normalized_forwards_selection(document)?;
        let forwards_range_start_offset = self.compute_start_offset(document);

        let mut chars_removed = 0;
        let mut is_first = true;
        let mut pointer_id = forwards_range.starting_token_id;
        let mut token_ids_to_remove = vec![];
        let tokens_collection = document.tokens_mut();

        loop {
            let result = {
                let Some(pointer) = tokens_collection.get_by_id(pointer_id) else {
                    return Err(format!("Error in SequentialTokenSelection.remove_deep: Unable to find token with id {} ({} chars in to removal)", pointer_id, chars_removed));
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
                                    // println!("KEEP: {}", number_of_chars_to_keep);
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
                        break;
                        // return Err(format!("Unable to remove SequentialTokenSelection: ran out of tokens (start at {} offset {} and went {} of {} chars)", forwards_range.starting_token_id, forwards_range.starting_token_offset, chars_removed, forwards_range.char_count));
                    }
                    is_first = false;
                },
                Err(err) => {
                    return Err(err)
                },
            }
        }

        for token_id in token_ids_to_remove.iter().rev() {
            tokens_collection.remove_leaf(*token_id)?;
        }

        document.tokens_mut().reset_caches_for_and_after(forwards_range_start_offset);
        document.clear_newline_cache_at(forwards_range_start_offset);

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
        )
    }

    // When called, converts the given SequentialTokenSelection into forwards form if it is a backwards
    // range. If the range is already forwards, then this is a no-op.
    pub fn as_forwards_selection(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        if !self.is_backwards {
            return Ok(self.clone());
        }

        let tokens_collection = document.tokens_mut();

        let mut start_offset = tokens_collection.compute_offset(self.starting_token_id);
        start_offset += self.starting_token_offset;
        // Add one because when going backwards, the "side" of the cursor is different...
        start_offset += 1;
        // ...however, ONLY if the cursor isn't at the end of the document
        if tokens_collection.get_by_offset(start_offset).is_none() {
            start_offset -= 1;
        }

        let end_offset = if self.char_count > start_offset {
            // This range seems to go to before the start of the document?
            0
        } else {
            start_offset - self.char_count
        };

        let Some((token, token_offset)) = tokens_collection.get_by_offset(end_offset) else {
            return Err(format!("Error in SequentialTokenSelection.as_forwards_selection: cannot get token at offset {} in tokens collection!", end_offset));
        };

        Ok(Self::new(
            token.id,
            token_offset,
            start_offset - end_offset,
        ))
    }

    // When called, normalizes the selection into a form that is easy to operate on:
    // 1. Converts the selection into forwards form
    // 2. Ensures that the starting token in the selection contains the `starting_token_offset`
    //    value (ie, the largest possible starting_token_offset is at the end of the starting_token)
    pub fn as_normalized_forwards_selection(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let forwards_selection = self.as_forwards_selection(document)?;
        let tokens_collection = document.tokens_mut();

        let mut starting_token_id = forwards_selection.starting_token_id;
        let mut starting_token_offset = forwards_selection.starting_token_offset;
        loop {
            let Some(starting_token) = tokens_collection.get_by_id(starting_token_id) else {
                return Err(format!("Error in SequentialTokenSelection.as_normalized_forwards_selection: unable to find token with id {starting_token_id}"));
            };

            let literal_text_length = if let Some(literal_text) = &starting_token.literal {
                literal_text.len()
            } else {
                0
            };

            if starting_token_offset < literal_text_length {
                break;
            }

            let Some(starting_token_next_id) = starting_token.next_id else {
                return Err(format!("Error in SequentialTokenSelection.as_normalized_forwards_selection: cannot find next token after {starting_token_id}!"));
            };

            starting_token_id = starting_token_next_id;
            starting_token_offset -= literal_text_length;
        }

        Ok(Self::new(starting_token_id, starting_token_offset, self.char_count))
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
        // println!("OFFSETS: {:?}", offsets);
        let Some(smallest_offset) = offsets.iter().min() else {
            return Err(format!("Error in SequentialTokenSelection.extend: Cannot get smallest offset in vec: {:?}", offsets));
        };
        let Some(largest_offset) = offsets.iter().max() else {
            return Err(format!("Error in SequentialTokenSelection.extend: Cannot get smallest offset in vec: {:?}", offsets));
        };
        let char_count = largest_offset - smallest_offset;
        // println!("CHAR COUNT? {} - {} = {}", largest_offset, smallest_offset, char_count);

        // NOTE: the direction of the resulting range is based off `range` so that if ranges of
        // different directions are put in, the most recent range is the one that dictates
        // direction
        if range.is_backwards {
            let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(*largest_offset) else {
                return Err(format!("Error in SequentialTokenSelection.extend: Cannot get token at offset {} in tokens collection!", largest_offset));
            };
            // println!("EXTEND RESULT: is_backwards=true char_count={char_count}");
            Ok(Self::new_backwards(start_token.id, start_token_offset, char_count))
        } else {
            let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(*smallest_offset) else {
                return Err(format!("Error in SequentialTokenSelection.extend: Cannot get token at offset {} in tokens collection!", smallest_offset));
            };
            // println!("EXTEND RESULT: is_backwards=false char_count={char_count}");
            Ok(Self::new(start_token.id, start_token_offset, char_count))
        }
    }

    // When called, scans forward in the token stream looking for whitespace, and returns a new
    // SequentialTokenSelection with the additional whitespace added to the end
    pub fn select_whitespace_after(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let range = self.as_normalized_forwards_selection(document)?;
        // println!("REMOVING SELECTION: {:?}", range);
        let tokens_collection = document.tokens_mut();

        // Start seeking from the end of the token forwards, looking for whitespace
        let mut end_offset = tokens_collection.compute_offset(range.starting_token_id);
        end_offset += range.starting_token_offset;
        end_offset += range.char_count;

        // Make sure that we aren't at the end of the document - if so, this is as far as the
        // selection will expand.
        // if tokens_collection.get_by_offset(end_offset+1).is_none() {
        //     return Ok(self.clone());
        // }
        if tokens_collection.get_by_offset(end_offset).is_none() {
            return Ok(self.clone());
        }
        document.seek_push(end_offset);

        let result = match document.read_forwards_until(|c, _| !is_whitespace_char(c), false, false) {
            Ok(Some((_range, _, selection))) => {
                // println!("NEW SELECTION: {:?}", selection);
                self.extend(document, selection)
            },
            // If the range cannot be extended (maybe we're at the end of the file?) then
            // just keep it as it is.
            Ok(None) => Ok(self.clone()),
            Err(e) => {
                // println!("RANGE: {:?}", e);
                Err(e)
            },
        };

        document.seek_pop();
        result
    }

    pub fn select_whitespace_before(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let range = self.as_normalized_forwards_selection(document)?;
        let tokens_collection = document.tokens_mut();

        // Start seeking from the start of the token backwards, looking for whitespace
        let mut start_offset = tokens_collection.compute_offset(range.starting_token_id);
        start_offset += range.starting_token_offset;

        // Make sure that we aren't at the start of the document - if so, this is as far as the
        // selection will expand.
        if start_offset == 0 {
            return Ok(self.clone());
        }
        document.seek_push(start_offset-1);

        let result = match document.read_backwards_until(|c, _| !is_whitespace_char(c), false, false) {
            Ok(Some((_, _, range))) => {
                self.extend(document, range)
            },
            // If the range cannot be extended (maybe we're at the start of the file?) then
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

    // Translates the given selection forwards / "to the right" the given number of characters. The
    // length of the selection remains the same.
    //
    // This returns `None` is the given selection move is impossible (ie, moving to after the
    // end of the document)
    pub fn move_forwards(&self, document: &mut Document, char_offset: usize) -> Option<Self> {
        let start_offset = self.compute_start_offset(document);

        let Some((token, token_offset)) = document.tokens_mut().get_by_offset(start_offset + char_offset) else {
            // This new offset is not a valid position in the document.
            return None;
        };

        let mut copy = self.clone();
        copy.starting_token_id = token.id;
        copy.starting_token_offset = token_offset;
        Some(copy)
    }

    // Translates the given selection backwards / "to the left" the given number of characters. The
    // length of the selection remains the same.
    //
    // This returns `None` is the given selection move is impossible (ie, moving to before the
    // start of the document)
    pub fn move_backwards(&self, document: &mut Document, char_offset: usize) -> Option<Self> {
        let start_offset = self.compute_start_offset(document);
        if char_offset > start_offset {
            // One cannot move to before the start of the document.
            return None;
        }

        if self.starting_token_offset > char_offset {
            // The starting token doesn't need to change, there's enough room in the start offset
            // to just subtract inline
            let mut copy = self.clone();
            copy.starting_token_offset -= char_offset;
            Some(copy)
        } else {
            // The starting token needs to change
            let Some((token, token_offset)) = document.tokens_mut().get_by_offset(start_offset) else {
                return None
            };
            let mut copy = self.clone();
            copy.starting_token_id = token.id;
            copy.starting_token_offset = token_offset;
            Some(copy)
        }
    }

    // When called, scans bcakwards in the token range, removing all whitespace characters at the
    // end of the SequentialTokenSelection, returning a new range with these changes.
    //
    // This is used in the `change` operation to preserve whitespace after a selection
    pub fn unselect_whitespace_after(
        &self,
        document: &mut Document,
    ) -> Result<Self, String> {
        let range = self.as_normalized_forwards_selection(document)?;
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
                // println!("MATCHED CHARS: {}", matched_chars);
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

    pub fn minimum_offset_extent(&self, document: &mut Document) -> usize {
        let range = self.range(document);
        std::cmp::min(range.start, range.end)
    }
    pub fn maximum_offset_extent(&self, document: &mut Document) -> usize {
        let range = self.range(document);
        std::cmp::max(range.start, range.end)
    }

    pub fn length_in_chars(&self, document: &mut Document) -> usize {
        self.maximum_offset_extent(document) - self.minimum_offset_extent(document)
    }

    pub fn compute_start_offset(&self, document: &mut Document) -> usize {
        let offset = document.tokens_mut().compute_offset(self.starting_token_id);
        offset + self.starting_token_offset
    }
    pub fn compute_final_offset(&self, document: &mut Document) -> usize {
        let start_offset = self.compute_start_offset(document);
        if self.is_backwards {
            if self.char_count > start_offset {
                // This is not good, the selection goes to before the start of the document...
                0
            } else {
                start_offset - self.char_count
            }
        } else {
            start_offset + self.char_count
        }
    }
}
