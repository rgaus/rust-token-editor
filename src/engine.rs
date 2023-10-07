use std::collections::HashMap;

use crate::token::*;
use crate::token_match_template::*;

const NEWLINE_CHAR: char = '\n';
fn is_word_char(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_uppercase()
}
fn is_whitespace_char(c: char) -> bool {
    c.is_ascii_whitespace()
}
fn is_other_char(c: char) -> bool {
    !is_word_char(c) && !is_whitespace_char(c)
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub struct SequentialTokenRange {
    pub starting_token_id: uuid::Uuid,
    pub starting_token_offset: usize,
    pub is_backwards: bool,
    pub char_count: usize,
}
impl SequentialTokenRange {
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

    // Creates a new SequentialTokenRange by taking two absolute offsets in the token stream
    pub fn new_from_offsets(
        buffer: &mut Buffer,
        offset_a: usize,
        offset_b: usize,
    ) -> Result<SequentialTokenRange, String> {
        let tokens_collection = buffer.tokens_mut();

        let (start_offset, end_offset) = if offset_a <= offset_b {
            (offset_a, offset_b)
        } else {
            (offset_b, offset_a)
        };

        let Some((start_token, start_token_offset)) = tokens_collection.get_by_offset(start_offset) else {
            return Err(format!("Cannot get start token at offset {} in document!", start_offset));
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
        buffer: &mut Buffer,
        keep_first_token: bool,
    ) -> Result<SequentialTokenRange, String> {
        let forwards_range = self.as_forwards_range(buffer)?;

        let mut chars_removed = 0;
        let mut is_first = true;
        let mut pointer_id = forwards_range.starting_token_id;
        let mut token_ids_to_remove = vec![];
        let tokens_collection = buffer.tokens_mut();

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
                        return Err(format!("Unable to remove SequentialTokenRange: ran out of tokens (start at {} offset {} and went {} of {} chars)", forwards_range.starting_token_id, forwards_range.starting_token_offset, chars_removed, forwards_range.char_count));
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
        buffer: &mut Buffer,
        new_text: String,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<Option<uuid::Uuid>, String> {
        let tokens_collection = buffer.tokens_mut();

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

    // When called, converts the given SequentialTokenRange into forwards form if it is a backwards
    // range. If the range is already forwards, then this is a no-op.
    pub fn as_forwards_range(
        &self,
        buffer: &mut Buffer,
    ) -> Result<SequentialTokenRange, String> {
        if !self.is_backwards {
            return Ok(self.clone());
        }

        let tokens_collection = buffer.tokens_mut();

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
            return Err(format!("Cannot get token at offset {} in document!", end_offset));
        };

        Ok(Self::new(
            token.id,
            token_offset,
            start_offset - end_offset,
        ))
    }

    // Given a second SequentialTokenRange, returns a copy of `self` expanded to fit the new
    // specified range. If the ranges do not form a continuous range, the gap between the ranges is
    // added to the resulting range.
    pub fn extend(
        &self,
        buffer: &mut Buffer,
        range: SequentialTokenRange,
    ) -> Result<SequentialTokenRange, String> {
        // NOTE: the below logic won't work unless the ranges are already forwards
        let existing_range = self.as_forwards_range(buffer)?;
        let new_range = range.as_forwards_range(buffer)?;

        let tokens_collection = buffer.tokens_mut();

        let mut existing_start_offset = tokens_collection.compute_offset(existing_range.starting_token_id);
        existing_start_offset += existing_range.starting_token_offset;
        let existing_end_offset = existing_start_offset + existing_range.char_count;

        let mut new_start_offset = tokens_collection.compute_offset(new_range.starting_token_id);
        new_start_offset += new_range.starting_token_offset;
        let new_end_offset = new_start_offset + new_range.char_count;

        let smaller_start_offset = std::cmp::min(existing_start_offset, new_start_offset);
        let larger_end_offset = std::cmp::max(existing_end_offset, new_end_offset);
        let char_count = larger_end_offset - smaller_start_offset;

        if existing_start_offset < new_start_offset {
            Ok(Self::new(
                existing_range.starting_token_id,
                existing_range.starting_token_offset,
                char_count,
            ))
        } else {
            Ok(Self::new(
                new_range.starting_token_id,
                new_range.starting_token_offset,
                char_count,
            ))
        }
    }

    // When called, scans forward in the token stream looking for whitespace, and returns a new
    // SequentialTokenRange with the additional whitespace added to the end
    pub fn select_whitespace_after(
        &self,
        buffer: &mut Buffer,
    ) -> Result<SequentialTokenRange, String> {
        let range = self.as_forwards_range(buffer)?;
        let tokens_collection = buffer.tokens_mut();

        // Start seeking from the end of the token forwards, looking for whitespace
        let mut end_offset = tokens_collection.compute_offset(range.starting_token_id);
        end_offset += range.starting_token_offset;
        end_offset += range.char_count;
        buffer.seek_push(end_offset);

        let result = match buffer.read_forwards_until(|c, _| !is_whitespace_char(c), false, false) {
            Ok(Some((_, _, range))) => self.extend(buffer, range),
            // If the range cannot be extended (maybe we're at the end of the file?) then
            // just keep it as it is.
            Ok(None) => Ok(self.clone()),
            Err(e) => Err(e),
        };

        buffer.seek_pop();
        result
    }

    // When called, scans bcakwards in the token range, removing all whitespace characters at the
    // end of the SequentialTokenRange, returning a new range with these changes.
    //
    // This is used in the `change` operation to preserve whitespace after a selection
    pub fn unselect_whitespace_after(
        &self,
        buffer: &mut Buffer,
    ) -> Result<SequentialTokenRange, String> {
        let range = self.as_forwards_range(buffer)?;
        let tokens_collection = buffer.tokens_mut();

        // Start seeking from the end of the token forwards, looking for whitespace
        let mut end_offset = tokens_collection.compute_offset(range.starting_token_id);
        end_offset += range.starting_token_offset;
        end_offset += range.char_count;
        buffer.seek_push(end_offset);

        let result = match buffer.read_backwards_until(
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

        buffer.seek_pop();
        result
    }
}

pub enum TraversalPattern {
    Left,
    Right,
    LowerWord,
    UpperWord,
    LowerBack,
    UpperBack,
    LowerEnd,
    UpperEnd,
    To(char),
    UpperTo(char),
    Find(char),
    UpperFind(char),
}

#[derive(Debug)]
pub struct Buffer {
    document: Box<TokensCollection>,
    offset_stack: Vec<usize>,

    // A cache that maps 1-indexed row to the offset at the start of that row.
    // This is used to quickly be able to convert from an offset to a (x, y) position and back
    // again
    newline_offset_cache: HashMap<usize, usize>,
}

impl Buffer {
    pub fn new_from_tokenscollection(document: Box<TokensCollection>) -> Self {
        Self {
            document: document,
            offset_stack: vec![0],
            newline_offset_cache: HashMap::new(),
        }
    }
    pub fn new_from_literal(literal: &str) -> Self {
        Self::new_from_tokenscollection(
            Box::new(TokensCollection::new_unparsed_literal(literal)),
        )
    }

    pub fn create_view(self) -> View {
        View::new(Box::new(self))
    }

    // Allow one to extract the token collection from the buffer for lower level mutations
    pub fn tokens(&self) -> &Box<TokensCollection> {
        &self.document
    }
    pub fn tokens_mut(&mut self) -> &mut Box<TokensCollection> {
        &mut self.document
    }

    pub fn seek_push(&mut self, offset: usize) {
        self.offset_stack.push(offset);
    }
    pub fn get_offset(&self) -> usize {
        let Some(last) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        *last
    }
    pub fn seek(&mut self, offset: usize) {
        let Some(last) = self.offset_stack.last_mut() else {
            panic!("offset_stack vector is empty!")
        };
        *last = offset;
    }
    pub fn seek_pop(&mut self) {
        if self.offset_stack.len() > 1 {
            self.offset_stack.pop();
        }
    }

    pub fn read(&mut self, number_of_chars: usize) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        let initial_offset = *offset;
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let token_id = token.id.clone();

        let result = self.document.stringify_for_offset(token_id, token_offset+number_of_chars);
        if result.len() >= token_offset+number_of_chars {
            let final_offset = initial_offset + number_of_chars;
            self.seek(final_offset);
            return Ok(Some((
                initial_offset..final_offset,
                String::from(&result[token_offset..token_offset+number_of_chars]),
                SequentialTokenRange::new(token_id, token_offset, final_offset),
            )));
        } else {
            // The buffer isn't long enough to return the full length of data, so return what we
            // can
            let chars = &result[token_offset..];
            let final_offset = initial_offset + chars.len();
            self.seek(final_offset);
            return Ok(Some((
                initial_offset..final_offset,
                String::from(chars),
                SequentialTokenRange::new(token_id, token_offset, final_offset),
            )));
        }
    }

    // Reads the buffer character by character until the passed `needle_func` passes, and then
    // returns the data that has been read.
    //
    // If `include_matched_char` is true, the final matched character is included, otherwise it is
    // not.
    //
    // If `should_match_at_end` is true, then if the match gets to the start of the token stream
    // and has not yet matched, automatically match at this point.
    pub fn read_forwards_until<F>(
        &mut self,
        mut needle_func: F,
        include_matched_char: bool,
        should_match_at_end: bool,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let initial_offset = *offset;

        let Some((token, token_offset)) = self.document.get_by_offset(initial_offset) else {
            return Err(format!("Cannot get token at offset {} in document!", initial_offset));
        };
        let token_id = token.id;
        // println!("TOK: {} -> {:?} {}", initial_offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.document.get_by_id(pointer_id) else {
                break;
            };

            if let Some(literal_text) = &pointer.literal {
                for (index, character) in literal_text.chars().enumerate() {
                    if is_first && index < token_offset {
                        continue;
                    }
                    println!("CHAR: {}", character);
                    result = format!("{}{}", result, character);
                    if needle_func(character, initial_offset + (result.len()-1)) {
                        is_done = true;
                        println!("DONE!");
                        break;
                    }
                }
            };

            if is_done {
                break;
            }

            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
            } else {
                break;
            }

            is_first = false;
        }

        let mut result_length = result.len();

        if !is_done {
            result_length += 1;

            // No character ever matched!
            if !should_match_at_end {
                return Ok(None);
            }
        };

        let final_offset = initial_offset + (result_length-1);

        if !include_matched_char && result_length > 0 {
            self.seek(final_offset);
            Ok(Some((
                initial_offset..final_offset,
                result[0..result_length-1].to_string(),
                SequentialTokenRange::new(token_id, token_offset, result_length-1),
            )))
        } else {
            self.seek(final_offset+1);
            println!("SEEK: {} => {}", initial_offset, final_offset);
            Ok(Some((
                initial_offset..final_offset+1,
                result,
                SequentialTokenRange::new(token_id, token_offset, result_length),
            )))
        }
    }

    // Reads the buffer character by character in reverse from the current location to the start of
    // the document until the specified `needle_func` passes, and then returns the data that has been read.
    //
    // If `include_matched_char` is true, the final matched character is included, otherwise it is
    // not.
    //
    // If `should_match_at_start` is true, then if the match gets to the start of the token stream
    // and has not yet matched, automatically match at this point.
    pub fn read_backwards_until<F>(
        &mut self,
        mut needle_func: F,
        include_matched_char: bool,
        should_match_at_start: bool,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        if *offset == 0 {
            // If already at the start, then there's nothing to match!
            return Ok(None);
        };
        let initial_offset = *offset - 1;
        let Some((token, mut token_offset)) = self.document.get_by_offset(initial_offset) else {
            return Err(format!("Cannot get token at offset {} in document!", initial_offset));
        };
        let token_id = token.id;
        println!("TOK: {} -> {:?} {}", initial_offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.document.get_by_id(pointer_id) else {
                break;
            };

            if let Some(literal_text) = &pointer.literal {
                for (end_index, character) in literal_text.chars().rev().enumerate() {
                    let index = (literal_text.len() - 1) - end_index;
                    // if is_first && index > token_offset-1 {
                    if is_first && index > token_offset {
                        continue;
                    }
                    result = format!("{}{}", character, result);
                    println!("FOO {} {} - {}", character, initial_offset, result.len()-1);
                    if needle_func(character, initial_offset - (result.len() - 1)) {
                        println!("NEEDLE DONE!");
                        is_done = true;
                        break;
                    }
                }
            };

            if is_done {
                break;
            }

            if let Some(prev_pointer_id) = pointer.previous_id {
                pointer_id = prev_pointer_id;
            } else {
                break;
            }

            is_first = false;
        }

        let mut result_length = result.len();
        let mut final_offset = initial_offset - (result_length-1);
        println!("BACK: '{}' {} - ({}-1)", result, initial_offset, result_length);

        if !is_done {
            println!("DONE!");
            // final_offset -= 1;

            // No character ever matched!
            if !should_match_at_start {
                return Ok(None);
            }
        };

        if !include_matched_char && final_offset > 0 && result_length > 0 {
            // if !is_done {
            //     self.seek(final_offset);
            // } else {
                self.seek(final_offset+1);
            // }

            if result.len() > 1 {
                // let mut result_text = result[1..].to_string();
                if !is_done {
                    self.seek(0);
                    Ok(Some((
                        initial_offset+1..0,
                        result,
                        SequentialTokenRange::new_backwards(token_id, token_offset, result_length),
                    )))
                } else {
                    Ok(Some((
                        initial_offset+1..final_offset+1,
                        result[1..].to_string(),
                        SequentialTokenRange::new_backwards(token_id, token_offset, result_length-1),
                    )))
                }
            } else {
                // One character might have been matched, but because `include_matched_char` is
                // false, this match results in being empty
                Ok(Some((
                    initial_offset+1..final_offset+1,
                    result[1..].to_string(),
                    SequentialTokenRange::new_backwards(token_id, token_offset, 0),
                )))
            }
        } else {
            if include_matched_char {
                self.seek(final_offset);
                Ok(Some((
                    initial_offset+1..final_offset,
                    result,
                    SequentialTokenRange::new_backwards(token_id, token_offset, result_length),
                )))
            } else {
                self.seek(final_offset);
                Ok(Some((
                    initial_offset+1..1,
                    result,
                    SequentialTokenRange::new_backwards(token_id, token_offset, result_length),
                )))
            }
        }
    }

    pub fn read_to_pattern(
        &mut self,
        pattern: TraversalPattern,
        repeat_count: usize,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> {
        let mut initial_offset = self.get_offset();
        let mut final_offset = initial_offset;

        let mut combined_result = String::from("");
        let mut combined_range: Option<SequentialTokenRange> = None;

        for index in 0..repeat_count {
            let mut hit_newline = false;
            let result = match pattern {
                TraversalPattern::Left => {
                    let offset = self.get_offset();
                    self.read_backwards_until(|c, i| {
                        if c == '\n' {
                            hit_newline = true;
                            true
                        } else {
                            offset > i
                        }
                    }, true, false)
                },
                TraversalPattern::Right => {
                    let offset = self.get_offset();
                    self.read_forwards_until(|c, i| {
                        if c == '\n' {
                            hit_newline = true;
                            true
                        } else {
                            offset < i
                        }
                    }, true, false)
                },
                TraversalPattern::LowerWord => {
                    // The current seek position is either a word char or not. Keep going
                    // forward until that classification changes.
                    let mut started_in_word: Option<bool> = None;
                    let mut hit_whitespace = false;
                    self.read_forwards_until(|c, _| {
                        let is_current_word_char = is_word_char(c);

                        // Once whitespace is reached, continue matching until that whitespace ends
                        if hit_whitespace && !is_whitespace_char(c) {
                            true
                        } else if is_whitespace_char(c) {
                            hit_whitespace = true;
                            false

                        } else if let Some(started_in_word) = started_in_word {
                            // Keep matching characters until whether it is a word char or not
                            // changes
                            started_in_word != is_current_word_char
                        } else {
                            // Is the first character a word character or not?
                            started_in_word = Some(is_current_word_char);
                            false
                        }
                    }, false, true)
                },
                TraversalPattern::UpperWord => {
                    // The current seek position is either a whitespace char or not. Keep going
                    // forward until that classification changes.
                    let mut started_in_whitespace: Option<bool> = None;
                    self.read_forwards_until(|c, _| {
                        let is_whitespace_char = is_whitespace_char(c);
                        if let Some(started_in_whitespace_val) = started_in_whitespace {
                            if started_in_whitespace_val && !is_whitespace_char {
                                true
                            } else if !started_in_whitespace_val && is_whitespace_char {
                                started_in_whitespace = Some(true);
                                false
                            } else {
                                false
                            }
                        } else {
                            started_in_whitespace = Some(is_whitespace_char);
                            false
                        }
                    }, false, true)
                },
                TraversalPattern::LowerBack => {
                    // The current seek position is either a word char or not. Keep going
                    // forward until that classification changes.
                    let mut is_first = true;
                    let mut first_char_was_other = false;
                    let mut second_char_was_whitespace: Option<bool> = None;
                    let mut finished_whitespace = false;
                    let mut started_in_word: Option<bool> = None;

                    self.read_backwards_until(|c, _| {
                        if is_first {
                            first_char_was_other = is_other_char(c);
                            is_first = false;
                            return false;
                        }
                        let is_current_word_char = is_word_char(c);

                        if first_char_was_other {
                            println!("IS OTHER {:?}", first_char_was_other);
                            !is_other_char(c)

                        // First, if there is immediately whitespace, scan through all of that
                        } else if second_char_was_whitespace.is_none() {
                            second_char_was_whitespace = Some(is_whitespace_char(c));

                            // If there isn't any whitespace as the second character, then skip
                            // past the prefixed whitespace section of the match
                            started_in_word = Some(is_current_word_char);
                            if is_whitespace_char(c) || is_other_char(c) {
                                true
                            } else {
                                finished_whitespace = true;
                                false
                            }

                        } else if second_char_was_whitespace == Some(true) && !finished_whitespace && is_whitespace_char(c) {
                            println!("HERE {:?}", second_char_was_whitespace);
                            finished_whitespace = false;
                            false
                        } else if second_char_was_whitespace == Some(true) && !finished_whitespace && !is_whitespace_char(c) {
                            // If the second char was whitespace, finish matching once the
                            // whitespace ends
                            true

                        } else if let Some(started_in_word) = started_in_word {
                            println!("------- {} != {}", started_in_word, is_current_word_char);
                            // Keep matching characters until whether it is a word char or not
                            // changes
                            started_in_word != is_current_word_char
                        } else {
                            // Is the first non whitespace character a word character or not?
                            started_in_word = Some(is_current_word_char);
                            false
                        }
                    }, false, true)
                },
                TraversalPattern::UpperBack => {
                    let mut is_first = true;
                    let mut second_char_was_whitespace: Option<bool> = None;
                    let mut finished_whitespace = false;

                    self.read_backwards_until(|c, _| {
                        if is_first {
                            is_first = false;
                            return false;
                        }
                        let is_current_not_whitespace_char = !is_whitespace_char(c);

                        // First, if there is immediately whitespace, scan through all of that
                        if second_char_was_whitespace.is_none() {
                            second_char_was_whitespace = Some(is_whitespace_char(c));

                            // If there isn't any whitespace as the second character, then skip
                            // past the prefixed whitespace section of the match
                            if !is_whitespace_char(c) {
                                finished_whitespace = true;
                                false
                            } else {
                                true
                            }

                        } else if second_char_was_whitespace == Some(true) && !finished_whitespace && is_whitespace_char(c) {
                            finished_whitespace = false;
                            false
                        } else if second_char_was_whitespace == Some(true) && !finished_whitespace && !is_whitespace_char(c) {
                            // If the second char was whitespace, finish matching once the
                            // whitespace ends
                            true

                        } else {
                            // Keep matching characters until another whitespace character shows up
                            // changes
                            is_whitespace_char(c)
                        }
                    }, false, true)
                },
                TraversalPattern::LowerEnd => {
                    let mut is_first = true;
                    let mut is_second = false;

                    let mut is_second_whitespace_char = false;
                    // Then follow the run of whitespace
                    let mut finished_leading_space = false;

                    let mut is_second_word_char = false;
                    // Then follow the run of word chars
                    let mut is_second_other_char = false;
                    // Then follow the run of other chars

                    self.read_forwards_until(|c, _| {
                        if is_first {
                            is_first = false;
                            is_second = true;
                            false
                        } else if is_second {
                            is_second = false;
                            is_second_whitespace_char = is_whitespace_char(c);
                            println!("Is second whitespace? {}", is_second_whitespace_char);
                            if !is_second_whitespace_char {
                                finished_leading_space = true;
                                is_second_word_char = is_word_char(c);
                                is_second_other_char = is_other_char(c);
                            }
                            false

                        // Follow a run of leading whitespace if the second character is a
                        // whitespace character
                        } else if is_second_whitespace_char && !finished_leading_space {
                            if is_whitespace_char(c) {
                                false
                            } else {
                                finished_leading_space = true;

                                is_second_word_char = is_word_char(c);
                                is_second_other_char = is_other_char(c);
                                false
                            }

                        // Stop matching once the type of character being matched changes
                        } else if is_second_word_char && !is_word_char(c) {
                            true
                        } else if is_second_other_char && !is_other_char(c) {
                            true

                        } else {
                            false
                        }
                    }, false, true)
                },
                TraversalPattern::UpperEnd => {
                    let mut finished_leading_space = false;
                    self.read_forwards_until(|c, _| {
                        // Read through a run of leading whitespace
                        if !finished_leading_space && is_whitespace_char(c) {
                            false
                        } else if !finished_leading_space && !is_whitespace_char(c) {
                            finished_leading_space = true;
                            false
                        } else {
                            // Otherwise though, stop at a whitespace character
                            is_whitespace_char(c)
                        }
                    }, false, true)
                },
                TraversalPattern::To(character) => {
                    self.read_forwards_until(|c, _| c == character, false, false)
                },
                TraversalPattern::UpperTo(character) => {
                    self.read_backwards_until(|c, _| c == character, false, false)
                },
                TraversalPattern::Find(character) => {
                    self.read_forwards_until(|c, _| c == character, true, false)
                },
                TraversalPattern::UpperFind(character) => {
                    self.read_backwards_until(|c, _| c == character, true, false)
                },
            };

            if hit_newline {
                break;
            }

            match result {
                Ok(Some((_, result, range))) => {
                    final_offset = self.get_offset();
                    println!("OFFSETS: {} {}", initial_offset, final_offset);

                    if final_offset > initial_offset {
                        combined_result = format!("{}{}", combined_result, result);
                    } else {
                        combined_result = format!("{}{}", result, combined_result);
                    }
                    if let Some(value) = combined_range {
                        combined_range = Some(value.extend(self, range)?);
                    } else {
                        println!("BACKWARDS: {:?}", range);
                        combined_range = Some(range.as_forwards_range(self)?);
                        println!("FORWARDS: {:?}", combined_range);
                    }
                },
                Ok(None) => {
                    // Couldn't find a next match, so stick with whatever we've got already
                    break;
                },
                Err(err) => {
                    return Err(err)
                },
            }
        }

        if initial_offset == final_offset || combined_range.is_none() {
            Ok(None)
        } else {
            Ok(Some((
                initial_offset..final_offset,
                combined_result,
                combined_range.unwrap(),
            )))
        }
    }

    // Remove all cached newlines that are at or after the given offset
    pub fn clear_newline_cache_at(&mut self, offset: usize) {
        self.newline_offset_cache.retain(|_, newline_start_offset| {
            *newline_start_offset < offset
        })
    }

    fn seed_newline_cache_if_empty(&mut self) {
        if self.newline_offset_cache.is_empty() {
            self.newline_offset_cache.insert(1, 0);
        }
    }

    // Given a (row, column) pair corresponding to a position in the final output token text,
    // returns the corresponding character offset that corresponds to that position.
    //
    // This function's behavior is undefined if the position is not a position that could possibly
    // exist in the token text (for example, if a row has x columns and (row, x+1) is passed)
    pub fn convert_rows_cols_to_offset(&mut self, position: (usize, usize)) -> usize {
        let (row, col) = position;
        if row == 0 || col == 0 {
            // Row and column are 1 indexed, so these values being set to 0 is impossible!
            return 0;
        }

        self.seed_newline_cache_if_empty();

        let (start_row, start_offset) = self.newline_offset_cache
            .iter()
            .fold((1, 0), |(previous_row, previous_offset), (cached_row, cached_offset)| {
                if *cached_row > row {
                    // This cached value is AFTER the value that is being queried for
                    (previous_row, previous_offset)
                } else if *cached_row > previous_row {
                    // Prefer the value that is later on in the list
                    (*cached_row, *cached_offset)
                } else {
                    (previous_row, previous_offset)
                }
            });
        // Work from start_offset reading character by character through the document until we are
        // at the right line

        // Then add `col_index` to get the offset

        let mut current_offset = start_offset;
        let mut current_row = start_row;
        self.seek_push(current_offset);
        while current_row < row {
            self.seek(current_offset);
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true, false) else {
                // There must not be an end of line for the rest of the text document
                // So we are done
                break;
            };

            let number_of_matched_chars_including_newline = result.len();
            current_offset += number_of_matched_chars_including_newline;
            current_row += 1;

            self.newline_offset_cache.insert(current_row, current_offset);
        }
        self.seek_pop();

        // So the column offset can just be added to the 0-indexed column offset
        current_offset + (col - 1)
    }

    // Given a character offset in the final output token text, returns the corresponding
    // one-indexed (row, column) pair that corresponds to that offset.
    //
    // This function's behavior is undefined if the offset passed is larger than the max token
    // offset within the token text.
    pub fn convert_offset_to_rows_cols(&mut self, offset: usize) -> (usize, usize) {
        self.seed_newline_cache_if_empty();

        let (start_row, start_offset) = self.newline_offset_cache
            .iter()
            .fold((1, 0), |(previous_row, previous_offset), (cached_row, cached_offset)| {
                if *cached_offset > offset {
                    // This cached offset is AFTER the value that is being queried for
                    (previous_row, previous_offset)
                } else if *cached_offset > previous_offset {
                    // Prefer the value that is later on in the list
                    (*cached_row, *cached_offset)
                } else {
                    (previous_row, previous_offset)
                }
            });

        let mut current_offset = start_offset;
        let mut current_row = start_row;
        self.seek_push(current_offset);
        while current_offset < offset {
            self.seek(current_offset);
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true, false) else {
                // There must not be an end of line for the rest of the text document
                // So we are done
                break;
            };

            let number_of_matched_chars_including_newline = result.len();
            if current_offset + number_of_matched_chars_including_newline > offset {
                // We went past the end of the line!
                break;
            }
            current_offset += number_of_matched_chars_including_newline;
            current_row += 1;

            self.newline_offset_cache.insert(current_row, current_offset);
        }
        self.seek_pop();

        let offset_difference = offset - current_offset;
        let column = offset_difference + 1; // NOTE: columns are one indexed!
        (current_row, column)
    }
}





#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum Mode {
    Normal,
    Insert,
    Visual,
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum Verb {
    Delete,
    Yank,
    Change,
    IndentRight,
    IndentLeft,
    AutoIndent,
    SwapCase,
    Uppercase,
    Lowercase,
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum Noun {
    Character,
    LowerWord,
    UpperWord,
    LowerBack,
    UpperBack,
    LowerEnd,
    UpperEnd,
    To(char),
    UpperTo(char),
    Find(char),
    UpperFind(char),
    Paragraph,
    Sentence,
    Line,
    BlockSquare,
    BlockParenthesis,
    BlockCurly,
    BlockAngle,
    BlockSquareOrParenthesis,
    BlockSquareOrCurly,
    BlockXMLTag,
    QuoteSingle,
    QuoteDouble,
    QuoteBacktick,
    Inside(Box<Noun>),
    Around(Box<Noun>),
}

#[derive(PartialEq)]
#[derive(Debug)]
#[derive(Clone)]
enum ViewState {
    Initial,
    HasVerb,
    IsInside,
    IsAround,
    PressedT,
    PressedUpperT,
    PressedF,
    PressedUpperF,
    PressedG(Box<ViewState>),
    Complete,
}

#[derive(Debug)]
pub struct View {
    buffer: Box<Buffer>,
    mode: Mode,

    // Position in the document in (rows, columns) format
    // (1, 1) is at the top left
    pub position: (usize, usize),

    // Command parsing state
    state: ViewState,
    command_count: String,
    is_backwards: bool,
    verb: Option<Verb>,
    noun: Option<Noun>,
    should_clear_command_count: bool,
}

#[derive(PartialEq)]
#[derive(Debug)]
pub struct ViewDumpedData {
    mode: Mode,
    command_count: usize,
    // is_backwards: bool,
    verb: Option<Verb>,
    noun: Option<Noun>,
}

impl View {
    pub fn new(buffer: Box<Buffer>) -> Self {
        Self {
            buffer: buffer,
            mode: Mode::Normal,
            position: (1, 1),
            state: ViewState::Initial,
            command_count: String::from(""),
            is_backwards: false,
            verb: None,
            noun: None,
            should_clear_command_count: false,
        }
    }
    fn clear_command(&mut self) {
        self.command_count = String::from("");
        self.is_backwards = false;
        self.verb = None;
        self.noun = None;
    }
    pub fn reset(&mut self) {
        self.state = ViewState::Initial;
        self.mode = Mode::Normal;
        self.clear_command();
    }

    pub fn dump(&self) -> ViewDumpedData {
        println!("-------------");
        println!("VIEW STATE:");
        println!("state={:?}", self.state);
        println!("mode={:?}", self.mode);
        println!("is_backwards={:?}", self.is_backwards);
        println!("command_count={:?}", self.command_count);
        println!("verb={:?}", self.verb);
        println!("noun={:?}", self.noun);
        println!("should_clear_command_count={:?}", self.should_clear_command_count);
        println!("-------------");

        ViewDumpedData {
            mode: self.mode.clone(),
            command_count: self.command_count.parse::<usize>().unwrap_or(1),
            // is_backwards: self.is_backwards,
            verb: self.verb.clone(),
            noun: self.noun.clone(),
        }
    }

    fn set_verb(&mut self, verb: Verb) {
        self.state = ViewState::HasVerb;
        self.verb = Some(verb);
        self.should_clear_command_count = true;
        println!("  SET VERB: {:?}", self.verb);
    }

    fn set_noun(&mut self, noun: Noun) {
        if self.state == ViewState::IsInside {
            self.noun = Some(Noun::Inside(Box::new(noun)));
        } else if self.state == ViewState::IsAround {
            self.noun = Some(Noun::Around(Box::new(noun)));
        } else {
            self.noun = Some(noun);
        }
        println!("  SET NOUN: {:?}", self.noun);
        self.state = ViewState::Complete;
    }

    // When this function return true, the next token being parsed can be a noun
    fn next_can_be_noun(&self) -> bool {
        self.state == ViewState::HasVerb || 
        self.state == ViewState::IsInside ||
        self.state == ViewState::IsAround
    }

    fn in_g_mode(&self) -> bool {
        if let ViewState::PressedG(_) = self.state {
            true
        } else {
            false
        }
    }

    // When called with string input, parses the input and calls `on_complete` every time that
    // a new command is successfully parsed
    pub fn raw_parse_input<F>(&mut self, input: &str, mut on_complete: F) where F: FnMut(&mut Self) {
        for character in input.chars() {
            // println!("CHAR: {}", character);
            match character {
                // TODO:
                // "+y - yank register
                // h / j / k / l - moving around
                // w / W / b / B / e / E - word+back+end
                // 0 / ^ / $ - start + end of line
                // f / F / t / T - to+until
                // gg / G / 123G - go to line number
                // % - matching brace
                //
                // p / P / "+p - paste
                // ]p / [p - paste in current indentation level
                // a / A / i / I / o / O / s / S / C / r / R - insert mode stuff
                // <C-C> / ESC - back to normal mode
                // v / V / <C-V> - visual mode
                // . - repeat last
                // <C-U> / <C-D> - half page up / half page down
                // <C-B> / <C-F> - page up / down

                // When the noun "t"/"T"/"f"/"F" is used, the enxt character refers to the
                // character that should be navigated to.
                c if self.state == ViewState::PressedT => self.set_noun(Noun::To(c)),
                c if self.state == ViewState::PressedUpperT => self.set_noun(Noun::UpperTo(c)),
                c if self.state == ViewState::PressedF => self.set_noun(Noun::Find(c)),
                c if self.state == ViewState::PressedUpperF => self.set_noun(Noun::UpperFind(c)),

                // "Verbs"
                'd' if self.state == ViewState::Initial => self.set_verb(Verb::Delete),
                'y' if self.state == ViewState::Initial => self.set_verb(Verb::Yank),
                'c' if self.state == ViewState::Initial => self.set_verb(Verb::Change),
                // '>' if self.state == ViewState::Initial => self.set_verb(Verb::IndentRight),
                // '<' if self.state == ViewState::Initial => self.set_verb(Verb::IndentLeft),
                // '=' if self.state == ViewState::Initial => self.set_verb(Verb::AutoIndent),
                'U' if self.in_g_mode() => self.set_verb(Verb::Uppercase),
                'u' if self.in_g_mode() => self.set_verb(Verb::Lowercase),

                // "inside" and "around" - ie, `cip`
                'i' if self.state == ViewState::HasVerb => { self.state = ViewState::IsInside },
                'a' if self.state == ViewState::HasVerb => { self.state = ViewState::IsAround },

                // Repeated Verbs - ie, `cc`, `gUU`
                'c' | 'd' | 'y' if self.state == ViewState::HasVerb => self.set_noun(Noun::Line),
                'U' if self.state == ViewState::HasVerb && self.verb == Some(Verb::Uppercase) => self.set_noun(Noun::Line),
                'u' if self.state == ViewState::HasVerb && self.verb == Some(Verb::Lowercase) => self.set_noun(Noun::Line),

                // "Nouns"
                'w' if self.next_can_be_noun() => self.set_noun(Noun::LowerWord),
                'p' if self.next_can_be_noun() => self.set_noun(Noun::Paragraph),
                's' if self.next_can_be_noun() => self.set_noun(Noun::Sentence),
                '[' | ']' if self.next_can_be_noun() => self.set_noun(Noun::BlockSquare),
                '(' | ')' if self.next_can_be_noun() => self.set_noun(Noun::BlockParenthesis),
                '{' | '}' if self.next_can_be_noun() => self.set_noun(Noun::BlockCurly),
                '<' | '>' if self.next_can_be_noun() => self.set_noun(Noun::BlockAngle),
                'b' if self.next_can_be_noun() => self.set_noun(Noun::BlockSquareOrParenthesis),
                'B' if self.next_can_be_noun() => self.set_noun(Noun::BlockSquareOrCurly),

                // Noun-like objects that are only valid after `i`nside or `a`round
                't' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockXMLTag)
                },
                '\'' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::QuoteSingle)
                },
                '"' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::QuoteDouble)
                },
                '`' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::QuoteBacktick)
                },

                // "Nouns"
                'w' if self.next_can_be_noun() => self.set_noun(Noun::LowerWord),
                'W' if self.next_can_be_noun() => self.set_noun(Noun::UpperWord),
                'b' if self.next_can_be_noun() => self.set_noun(Noun::LowerBack),
                'B' if self.next_can_be_noun() => self.set_noun(Noun::UpperBack),
                'e' if self.next_can_be_noun() => self.set_noun(Noun::LowerEnd),
                'E' if self.next_can_be_noun() => self.set_noun(Noun::UpperEnd),

                't' if self.next_can_be_noun() => self.state = ViewState::PressedT,
                'T' if self.next_can_be_noun() => self.state = ViewState::PressedUpperT,
                'f' if self.next_can_be_noun() => self.state = ViewState::PressedF,
                'F' if self.next_can_be_noun() => self.state = ViewState::PressedUpperF,

                // A number: adjust the number of times the command should be run
                '0'..='9' => {
                    if self.should_clear_command_count {
                        self.command_count = String::from("");
                    }
                    self.command_count = format!("{}{}", self.command_count, character);
                    self.should_clear_command_count = false;
                },

                // `g` is weird, it's used as a prefix for a lot of other commands
                // So go into a different mode once it is pressed
                'g' => {
                    self.state = ViewState::PressedG(Box::new(self.state.clone()));
                },

                // `x` is kinda weird, it's both a noun and a verb
                'x' if self.state == ViewState::Initial => {
                    self.set_verb(Verb::Delete);
                    self.set_noun(Noun::Character);
                },

                // If an unknown character was specified for this part in a command, reset back to
                // the start
                _ => {
                    println!("RESET!");
                    self.reset();
                },
            }

            if self.state == ViewState::Complete {
                on_complete(self);
            }
        }
    }

    pub fn process_input(&mut self, input: &str) {
        self.raw_parse_input(input, |inner_self| {
            // Once a command has completed processing, execute it!
            inner_self.execute_command();
        })
    }

    fn execute_command(&mut self) {
        if self.state != ViewState::Complete {
            return;
        }

        let command_count = self.command_count.parse::<usize>().unwrap_or(1);
        let range = match self.noun {
            Some(Noun::Character) => {
                if self.is_backwards {
                    self.buffer.read_to_pattern(TraversalPattern::Left, command_count)
                } else {
                    self.buffer.read_to_pattern(TraversalPattern::Right, command_count)
                }
            },
            Some(Noun::LowerWord) => self.buffer.read_to_pattern(TraversalPattern::LowerWord, command_count),
            Some(Noun::UpperWord) => self.buffer.read_to_pattern(TraversalPattern::UpperWord, command_count),
            Some(Noun::LowerBack) => self.buffer.read_to_pattern(TraversalPattern::LowerBack, command_count),
            Some(Noun::UpperBack) => self.buffer.read_to_pattern(TraversalPattern::UpperBack, command_count),
            Some(Noun::LowerEnd) => self.buffer.read_to_pattern(TraversalPattern::LowerEnd, command_count),
            Some(Noun::UpperEnd) => self.buffer.read_to_pattern(TraversalPattern::UpperEnd, command_count),
            Some(Noun::To(c)) => self.buffer.read_to_pattern(TraversalPattern::To(c), command_count),
            Some(Noun::UpperTo(c)) => self.buffer.read_to_pattern(TraversalPattern::UpperTo(c), command_count),
            Some(Noun::Find(c)) => self.buffer.read_to_pattern(TraversalPattern::Find(c), command_count),
            Some(Noun::UpperFind(c)) => self.buffer.read_to_pattern(TraversalPattern::UpperFind(c), command_count),
            // Paragraph,
            // Sentence,
            // Line,
            // BlockSquare,
            // BlockParenthesis,
            // BlockCurly,
            // BlockAngle,
            // BlockSquareOrParenthesis,
            // BlockSquareOrCurly,
            // BlockXMLTag,
            // QuoteSingle,
            // QuoteDouble,
            // QuoteBacktick,
            // Inside(Box<Noun>),
            // Around(Box<Noun>),
            _ => self.buffer.read(1),
        };

        self.dump();
        self.clear_command();
    }
}


#[cfg(test)]
mod test_engine {
    use regex::Regex;
    use super::*;

    fn remove_sequentialtokenrange(v: Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String>) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
    )>, String> {
        match v {
            Ok(a) => {
                if let Some((b, c, d)) = a {
                    println!("D: {:?}", d);
                    Ok(Some((b, c)))
                } else {
                    Ok(None)
                }
            }
            Err(e) => Err(e),
        }
    }

    mod test_buffer {
        use super::*;

        // This mini language is either:
        // - A single `1`
        // - Between 1 to 3 `1`s, followed by a `2`
        fn initialize_mini_language_twelve() -> HashMap<&'static str, TokenMatchTemplate> {
            let mut token_match_templates_map = HashMap::new();
            token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::any(vec![
                    TokenMatchTemplateMatcher::reference("Twelve"),
                    TokenMatchTemplateMatcher::reference("One"),
                ]),
            ]));
            token_match_templates_map.insert("Twelve", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::repeat_count(
                    Box::new(TokenMatchTemplateMatcher::reference("One")),
                    1, 3
                ),
                TokenMatchTemplateMatcher::raw("2"),
            ]));
            token_match_templates_map.insert("One", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::regex(
                    Regex::new(r"^(?<one>1)").unwrap(),
                ),
            ]));

            token_match_templates_map
        }

        #[test]
        fn it_is_able_to_read_from_buffer_one_at_a_time() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            // Get a few subranges to make sure they generate the right data
            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(0);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((0..3, String::from("111")))));

            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(1);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((1..4, String::from("112")))));

            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(2);
            assert_eq!(remove_sequentialtokenrange(buffer.read(2)), Ok(Some((2..4, String::from("12")))));

            // Make sure that if at the end, as much data is returned as possible
            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(3);
            assert_eq!(remove_sequentialtokenrange(buffer.read(5)), Ok(Some((3..4, String::from("2")))));
        }

        #[test]
        fn it_is_able_to_read_from_buffer_repeatedly() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
            assert_eq!(result.0, TokenParseStatus::FullParse); // status

            // Make sure the parser parsed the input that was expected
            assert_eq!(result.4.stringify(), "1112");

            // Get a few subranges to make sure they generate the right data
            let mut buffer = Buffer::new_from_tokenscollection(Box::new(result.4));
            buffer.seek(0);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((0..3, String::from("111")))));
            buffer.seek(1);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((1..4, String::from("112")))));
            buffer.seek(2);
            assert_eq!(remove_sequentialtokenrange(buffer.read(2)), Ok(Some((2..4, String::from("12")))));

            // Make sure that if at the end, as much data is returned as possible
            buffer.seek(3);
            assert_eq!(remove_sequentialtokenrange(buffer.read(5)), Ok(Some((3..4, String::from("2")))));
        }

        #[test]
        fn it_is_able_to_read_from_plain_text_buffer_repeatedly() {
            // Get a few subranges to make sure they generate the right data
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            buffer.seek(0);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((0..3, String::from("foo")))));
            buffer.seek(1);
            assert_eq!(remove_sequentialtokenrange(buffer.read(3)), Ok(Some((1..4, String::from("oo ")))));
            buffer.seek(2);
            assert_eq!(remove_sequentialtokenrange(buffer.read(5)), Ok(Some((2..7, String::from("o bar")))));
            buffer.seek(6);
            assert_eq!(remove_sequentialtokenrange(buffer.read(4)), Ok(Some((6..10, String::from("r ba")))));

            // Make sure that if at the end, as much data is returned as possible
            buffer.seek(9);
            assert_eq!(remove_sequentialtokenrange(buffer.read(10)), Ok(Some((9..11, String::from("az")))));
        }

        mod read_forwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == 'b', true, false)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == 'b', false, false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(buffer.get_offset(), 4);
            }

            #[test]
            fn it_should_seek_by_index_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|_, i| i >= 5, true, false)),
                    Ok(Some((0..6, "foo ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 6);
            }

            #[test]
            fn it_should_seek_by_index_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|_, i| i >= 5, false, false)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);
            }

            #[test]
            fn it_should_never_match_a_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == 'X', false, false)),
                    Ok(None)
                );
                assert_eq!(buffer.get_offset(), 0);
            }

            #[test]
            fn it_should_seek_forward_in_sequence() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");

                // First seek to the first space
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == ' ', true, false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(buffer.get_offset(), 4);

                // Then seek to right before the `a`
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == 'a', false, false)),
                    Ok(Some((4..5, "b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);

                // Then seek by index most of the way to the end
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_forwards_until(|_, i| i >= 9, true, false)),
                    Ok(Some((5..10, "ar ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 10);
            }
        }

        mod read_backwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                buffer.seek(10);
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_backwards_until(|c, _| c == 'r', true, false)),
                    Ok(Some((10..6, "r ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 6);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                buffer.seek(10);
                assert_eq!(
                    remove_sequentialtokenrange(buffer.read_backwards_until(|c, _| c == 'r', false, false)),
                    Ok(Some((10..7, " ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 7);
            }
        }

        #[test]
        fn it_should_seek_forward_and_backwards_in_sequence() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");

            // First seek to the first space
            assert_eq!(
                remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == ' ', true, false)),
                Ok(Some((0..4, "foo ".to_string())))
            );
            assert_eq!(buffer.get_offset(), 4);

            // Then seek back a few characters
            assert_eq!(
                remove_sequentialtokenrange(buffer.read_backwards_until(|c, _| c == 'f', true, false)),
                Ok(Some((4..0, "foo ".to_string())))
            );
            assert_eq!(buffer.get_offset(), 0);

            // Then seek to the first space, NOT INCLUDING IT
            assert_eq!(
                remove_sequentialtokenrange(buffer.read_forwards_until(|c, _| c == ' ', false, false)),
                Ok(Some((0..3, "foo".to_string())))
            );
            assert_eq!(buffer.get_offset(), 3);

            // FIXME: this is not working
            // // Then seek back a few characters again, NOT INCLUDING IT
            // assert_eq!(
            //     remove_sequentialtokenrange(buffer.read_backwards_until(|c, _| c == 'f', false, false)),
            //     Ok(Some((3..1, "oo".to_string())))
            // );
            // assert_eq!(buffer.get_offset(), 1);
        }

        mod read_to_pattern {
            use super::*;

            mod lower_word {
                use super::*;

                #[test]
                fn it_should_change_lower_word_then_whitespace_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo bar baz");

                    // Get the first lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..4);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 4);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_lower_word_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");

                    // Get the first lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // When performing a change, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), ".foo bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST.foo bar baz");
                }

                #[test]
                fn it_should_change_lower_word_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar   baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..14);
                    assert_eq!(matched_chars, "bar   ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 6);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo    baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST   baz");
                }

                #[test]
                fn it_should_change_lower_word_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 12);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_change_2_lower_words_in_middle_right_up_to_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        2,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..15);
                    assert_eq!(matched_chars, "bar baz");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 7);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_lower_words_at_end_and_run_out_of_chars() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        // NOTE: there isn't enough characters for three words! Only two.
                        // But, it matches to the end anyway.
                        3,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..15);
                    assert_eq!(matched_chars, "bar baz");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 7);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_lower_words_at_end_and_spill_over_to_next_line() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz\nqux quux");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        3, // NOTE: this spills over to the next line!
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..20);
                    assert_eq!(matched_chars, "bar baz\nqux ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 12);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 11);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST quux");
                }

                #[test]
                fn it_should_change_lower_words_at_end_of_string_ending_with_whitespace() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz     ");
                    buffer.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..20);
                    assert_eq!(matched_chars, "baz     ");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 8);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 12);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar      ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TEST     ");
                }
            }

            mod upper_word {
                use super::*;

                #[test]
                fn it_should_change_upper_word_then_whitespace_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");

                    // Get the first upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..8);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 8);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_word_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");

                    // Get the first upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..8);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 8);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new()).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_word_with_punctuation_and_spaces_after_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar..b_ar   baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..20);
                    assert_eq!(matched_chars, "bar..b_ar   ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 12);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 9);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo    baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST   baz");
                }

                #[test]
                fn it_should_change_upper_word_with_numbers_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz1!");
                    buffer.seek(12); // Move to the start of "baz"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..17);
                    assert_eq!(matched_chars, "baz1!");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 5);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 12);
                    assert_eq!(modified_selection.char_count, 5);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_change_2_upper_words_in_middle_right_up_to_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar.bar baz.baz quux");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        2,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..24);
                    assert_eq!(matched_chars, "bar.bar baz.baz ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 16);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 15);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST quux");
                }

                #[test]
                fn it_should_change_3_upper_words_at_end_and_run_out_of_chars() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar.bar baz.baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        // NOTE: there isn't enough characters for three words! Only two.
                        // But, it matches to the end anyway.
                        3,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..23);
                    assert_eq!(matched_chars, "bar.bar baz.baz");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 15);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    //
                    // NOTE: for this case, there is no whitespace after the token, so this is a no-op
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 15);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_upper_words_at_end_and_spill_over_to_next_line() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz.baz\nqux.qux quux");
                    buffer.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperWord,
                        3, // NOTE: this spills over to the next line!
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..28);
                    assert_eq!(matched_chars, "bar baz.baz\nqux.qux ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 20);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut buffer).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 19);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo TEST quux");
                }
            }

            mod lower_back {
                use super::*;

                #[test]
                fn it_should_change_lower_back_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo bar baz");
                    buffer.seek(4); // Move to the start of "bar"

                    // Get the first lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerBack,
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 4..0);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Go back a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerBack,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..4);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.starting_token_offset, 4);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(14); // Move to the end of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerBack,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 14..12);
                    assert_eq!(matched_chars, "ba");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 2);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar z");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TESTz");
                }

                #[test]
                fn it_should_seek_repeatedly() {
                    // First space          ----> "TEST bar.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(3);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 3..0);
                        assert_eq!(matched_chars, "foo");
                        assert_eq!(selection.starting_token_offset, 0);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), " bar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "TEST bar.baaaaar baz");
                    }

                    // First char of "bar"  ----> "TESTbar.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(4);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 4..0);
                        assert_eq!(matched_chars, "foo ");
                        assert_eq!(selection.starting_token_offset, 0);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "bar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "TESTbar.baaaaar baz");
                    }

                    // Second char of "bar"  ----> "foo TESTar.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(5);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 5..4);
                        assert_eq!(matched_chars, "b");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo ar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo TESTar.baaaaar baz");
                    }

                    // Third char of "bar"  -> "foo TESTr.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(6);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 6..4);
                        assert_eq!(matched_chars, "ba");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo r.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo TESTr.baaaaar baz");
                    }

                    // Period               -> "foo TEST.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(7);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 7..4);
                        assert_eq!(matched_chars, "bar");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo .baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo TEST.baaaaar baz");
                    }

                    // First char of "baaa" -> "foo barTESTbaaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(8);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 8..7);
                        assert_eq!(matched_chars, ".");
                        assert_eq!(selection.starting_token_offset, 7);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo barbaaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo barTESTbaaaaar baz");
                    }

                    // Second char of "baaa" > "foo bar.TESTaaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(9);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 9..8);
                        assert_eq!(matched_chars, "b");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.aaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.TESTaaaaar baz");
                    }

                    // Third char of "baaa" > "foo bar.TESTaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(10);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 10..8);
                        assert_eq!(matched_chars, "ba");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.aaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.TESTaaaar baz");
                    }

                    // Space after "baaaar" > "foo bar.TEST baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(15);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerBack,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 15..8);
                        assert_eq!(matched_chars, "baaaaar");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 7);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar. baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.TEST baz");
                    }
                }
            }

            mod upper_back {
                use super::*;

                #[test]
                fn it_should_change_upper_back_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo bar baz");
                    buffer.seek(4); // Move to the start of "bar"

                    // Go back an upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperBack,
                        1,
                    ).unwrap().unwrap();
                    println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 4..0);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(8); // Move to the start of "bar"

                    // Go back an upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperBack,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..0);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 8);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(14); // Move to the end of "baz"

                    // Go back an upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperBack,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 14..12);
                    assert_eq!(matched_chars, "ba");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 2);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar z");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TESTz");
                }
            }

            mod lower_end {
                use super::*;

                #[test]
                fn it_should_change_lower_end_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo bar baz");

                    // Go to the end of the first word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_lower_end_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo bar.bar baz");
                    buffer.seek(4); // Move to the start of "bar.bar"

                    // Get the end of the second word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 4..7);
                    assert_eq!(matched_chars, "bar");
                    assert_eq!(selection.starting_token_offset, 4);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo .bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo TEST.bar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_seek_repeatedly() {
                    // First space          ----> "fooTEST.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(3);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 3..7);
                        assert_eq!(matched_chars, " bar");
                        assert_eq!(selection.starting_token_offset, 3);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "fooTEST.baaaaar baz");
                    }

                    // First char of "bar"  ----> "foo TEST.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(4);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 4..7);
                        assert_eq!(matched_chars, "bar");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo .baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo TEST.baaaaar baz");
                    }

                    // Second char of "bar"  ----> "foo TESTar.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(5);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 5..7);
                        assert_eq!(matched_chars, "ar");
                        assert_eq!(selection.starting_token_offset, 5);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo b.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bTEST.baaaaar baz");
                    }

                    // Third char of "bar"  -> "foo baTESTbaaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(6);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 6..8);
                        assert_eq!(matched_chars, "r.");
                        assert_eq!(selection.starting_token_offset, 6);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo babaaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo baTESTbaaaaar baz");
                    }

                    // Period               -> "foo TEST.baaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(7);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 7..15);
                        assert_eq!(matched_chars, ".baaaaar");
                        assert_eq!(selection.starting_token_offset, 7);
                        assert_eq!(selection.char_count, 8);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo barTEST baz");
                    }

                    // First char of "baaa" -> "foo barTESTbaaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(8);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 8..15);
                        assert_eq!(matched_chars, "baaaaar");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 7);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar. baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.TEST baz");
                    }

                    // Second char of "baaa" > "foo bar.TESTaaaaar baz"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(9);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 9..15);
                        assert_eq!(matched_chars, "aaaaar");
                        assert_eq!(selection.starting_token_offset, 9);
                        assert_eq!(selection.char_count, 6);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.b baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.bTEST baz");
                    }

                    // Space after "baaaar" > "foo bar.baaaaarTEST"
                    {
                        let mut buffer = Buffer::new_from_literal("foo bar.baaaaar baz");
                        buffer.seek(15);
                        let (range, matched_chars, selection) = buffer.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 15..19);
                        assert_eq!(matched_chars, " baz");
                        assert_eq!(selection.starting_token_offset, 15);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.baaaaar");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                        assert_eq!(buffer.tokens_mut().stringify(), "foo bar.baaaaarTEST");
                    }
                }
            }

            mod upper_end {
                use super::*;

                #[test]
                fn it_should_change_upper_end_at_start() {
                    let mut buffer = Buffer::new_from_literal("foo bar baz");

                    // Go to the end of the first word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_end_in_middle() {
                    let mut buffer = Buffer::new_from_literal("foo bar.bar baz");
                    buffer.seek(4); // Move to the start of "bar.bar"

                    // Get the end of the second word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 4..11);
                    assert_eq!(matched_chars, "bar.bar");
                    assert_eq!(selection.starting_token_offset, 4);
                    assert_eq!(selection.char_count, 7);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo  baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo TEST baz");
                }

                #[test]
                fn it_should_change_upper_back_at_end() {
                    let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
                    buffer.seek(12); // Move to the start of "baz"

                    // Get a upper word
                    let (range, matched_chars, selection) = buffer.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut buffer, true).unwrap();
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut buffer, String::from("TEST"), &HashMap::new());
                    assert_eq!(buffer.tokens_mut().stringify(), "foo.foo bar TEST");
                }
            }
        }
    }

    mod test_view {
        use super::*;

        #[test]
        fn it_should_parse_many_different_sequences() {
            let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
            let mut view = buffer.create_view();

            for (input_text, dumped_data) in vec![
                ("w", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: None,
                    noun: Some(Noun::LowerWord),
                }),
                ("2w", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: None,
                    noun: Some(Noun::LowerWord),
                }),
                ("5B", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 5,
                    verb: None,
                    noun: Some(Noun::UpperBack),
                }),

                ("cw", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::LowerWord),
                }),
                ("2cw", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::LowerWord),
                }),
                ("c2w", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::LowerWord),
                }),

                // Inside / Around
                ("cip", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Inside(Box::new(Noun::Paragraph))),
                }),
                ("ca{", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Around(Box::new(Noun::BlockCurly))),
                }),
                ("ca<", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Around(Box::new(Noun::BlockAngle))),
                }),
                ("ci'", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Inside(Box::new(Noun::QuoteSingle))),
                }),

                // Linewise operations
                ("dd", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::Line),
                }),
                ("3cc", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 3,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Line),
                }),
                ("5guu", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 5,
                    verb: Some(Verb::Lowercase),
                    noun: Some(Noun::Line),
                }),
                ("gUU", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Uppercase),
                    noun: Some(Noun::Line),
                }),
                ("12gUU", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 12,
                    verb: Some(Verb::Uppercase),
                    noun: Some(Noun::Line),
                }),

                // "x" command
                ("x", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::Character),
                }),
                ("3x", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 3,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::Character),
                }),
                // TODO: "cx" should not work

                ("db", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::LowerBack),
                }),
                ("d5b", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 5,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::LowerBack),
                }),
                ("dB", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperBack),
                }),
                ("de", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::LowerEnd),
                }),
                ("2dE", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperEnd),
                }),

                // "t"/"T"/"f"/"F" commands
                ("dte", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::To('e')),
                }),
                ("dTe", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperTo('e')),
                }),
                ("dfe", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::Find('e')),
                }),
                ("dFe", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperFind('e')),
                }),
                ("d2Fe", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperFind('e')),
                }),
                ("2dFe", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::UpperFind('e')),
                }),
                ("dff", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::Find('f')),
                }),
            ] {
                view.raw_parse_input(input_text, |_| {});
                assert_eq!(view.dump(), dumped_data, "Assertion failed: `{}`", input_text);
                view.reset();
            }
        }
    }
}
