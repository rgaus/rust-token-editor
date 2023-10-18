use std::collections::HashMap;
use std::ops::Index;

use crate::token::*;
use crate::token_match_template::*;
use crate::token_selection::*;
use crate::text_utils::*;

pub enum TraversalPattern {
    Left,
    Right,
    LowerWord,
    UpperWord,
    LowerBack,
    UpperBack,
    LowerEnd,
    UpperEnd,
    LowerBackEnd,
    UpperBackEnd,
    MatchingDelimiter,
    To(char),
    UpperTo(char),
    Find(char),
    UpperFind(char),
}

#[derive(Debug)]
pub struct Document {
    tokens_collection: Box<TokensCollection>,
    offset_stack: Vec<usize>,

    // A cache that maps 1-indexed row to the offset at the start of that row.
    // This is used to quickly be able to convert from an offset to a (x, y) position and back
    // again
    newline_offset_cache: HashMap<usize, usize>,
}

impl Document {
    pub fn new_from_tokenscollection(tokens_collection: Box<TokensCollection>) -> Self {
        Self {
            tokens_collection: tokens_collection,
            offset_stack: vec![0],
            newline_offset_cache: HashMap::new(),
        }
    }
    pub fn new_from_literal(literal: &str) -> Self {
        Self::new_from_tokenscollection(
            Box::new(TokensCollection::new_unparsed_literal(literal)),
        )
    }

    pub fn create_buffer(self) -> Buffer {
        Buffer::new(Box::new(self))
    }

    // Allow one to extract the token collection from the document for lower level mutations
    pub fn tokens(&self) -> &Box<TokensCollection> {
        &self.tokens_collection
    }
    pub fn tokens_mut(&mut self) -> &mut Box<TokensCollection> {
        &mut self.tokens_collection
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
        SequentialTokenSelection, /* The token range that was matched */
    )>, String> {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.tokens_collection.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in tokens collection!", offset));
        };
        let initial_offset = *offset;
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let token_id = token.id.clone();

        let result = self.tokens_collection.stringify_for_offset(token_id, token_offset+number_of_chars);
        if result.len() >= token_offset+number_of_chars {
            let final_offset = initial_offset + number_of_chars;
            self.seek(final_offset);
            return Ok(Some((
                initial_offset..final_offset,
                String::from(&result[token_offset..token_offset+number_of_chars]),
                SequentialTokenSelection::new(token_id, token_offset, final_offset),
            )));
        } else {
            // The document isn't long enough to return the full length of data, so return what we
            // can
            let chars = &result[token_offset..];
            let final_offset = initial_offset + chars.len();
            self.seek(final_offset);
            return Ok(Some((
                initial_offset..final_offset,
                String::from(chars),
                SequentialTokenSelection::new(token_id, token_offset, final_offset),
            )));
        }
    }

    // Read forward in the document and if the next characters match the string `pattern`, then
    // return the match. Otherwise, returns `Ok(None)`.
    pub fn read_if_matches(&mut self, pattern: &str) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenSelection, /* The token range that was matched */
    )>, String> {
        let initial_offset = self.get_offset();

        match self.read(pattern.len()) {
            Ok(Some((range, text, selection))) => {
                if text == pattern {
                    Ok(Some((range, text, selection)))
                } else {
                    self.seek(initial_offset);
                    Ok(None)
                }
            },
            Ok(None) => Ok(None),
            Err(err) => Err(err),
        }
    }

    fn read_forwards_and_backwards_to_find_backslashes(&mut self) -> usize {
        let initial_offset = self.get_offset();
        let mut backslash_count = 0;

        // First, try to read backwards looking for backslashes
        if initial_offset > 0 {
            self.seek(initial_offset - 1);
            while let Ok(Some(_)) = self.read_if_matches("\\") {
                backslash_count += 1;
                self.seek(self.get_offset() - 2);
            }
        }

        // Then, try to read fowards
        self.seek(initial_offset);
        while let Ok(Some(_)) = self.read_if_matches("\\") {
            backslash_count += 1;
        }

        println!("BACKSLASH COUNT: {} offset={}", backslash_count, self.get_offset());
        backslash_count
    }

    // pub fn read_backwards(&mut self, number_of_chars: usize) -> Result<Option<(
    //     std::ops::Range<usize>, #<{(| The matched token offset range |)}>#
    //     String, #<{(| The matched literal data in all tokens, concatenated |)}>#
    //     SequentialTokenSelection, #<{(| The token range that was matched |)}>#
    // )>, String> {
    //     let Some(offset) = self.offset_stack.last() else {
    //         panic!("offset_stack vector is empty!")
    //     };
    //     let initial_offset = *offset;
    //
    //     let final_offset = if number_of_chars > initial_offset {
    //         initial_offset - number_of_chars
    //     } else {
    //         0
    //     };
    //     self.seek(final_offset);
    //
    //     let Some((initial_token, initial_token_offset)) = self.tokens_collection.get_by_offset(initial_offset) else {
    //         return Err(format!("Cannot get token at offset {} in tokens collection!", initial_offset));
    //     };
    //     let initial_token_id = initial_token.id.clone();
    //
    //     let Some((token, token_offset)) = self.tokens_collection.get_by_offset(final_offset) else {
    //         return Err(format!("Cannot get token at offset {} in tokens collection!", final_offset));
    //     };
    //     let token_id = token.id.clone();
    //
    //     let result = self.tokens_collection.stringify_for_offset(token_id, token_offset+number_of_chars);
    //     if result.len() >= token_offset+number_of_chars {
    //         let final_offset = initial_offset + number_of_chars;
    //         return Ok(Some((
    //             final_offset..initial_offset,
    //             String::from(&result[initial_token_offset..initial_token_offset+number_of_chars]),
    //             SequentialTokenSelection::new_backwards(initial_token_id, initial_token_offset, number_of_chars),
    //         )));
    //     } else {
    //         // The document isn't long enough to return the full length of data, so return what we
    //         // can
    //         let chars = &result[token_offset..];
    //         let final_offset = initial_offset + chars.len();
    //         return Ok(Some((
    //             final_offset..initial_offset,
    //             String::from(chars),
    //             SequentialTokenSelection::new_backwards(initial_token_id, initial_token_offset, number_of_chars),
    //         )));
    //     }
    // }

    // Reads the document character by character until the passed `needle_func` passes, and then
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
        SequentialTokenSelection, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let mut initial_offset = *offset;

        let Some((token, token_offset)) = self.tokens_collection.get_by_offset(initial_offset) else {
            // If at the start of the document, the document may just be empty
            // So fail gracefully in this case
            if initial_offset == 0 {
                return Ok(None)
            }
            return Err(format!("Cannot get token at offset {} in tokens collection!", initial_offset));
        };
        let token_id = token.id;
        // println!("TOK: {} -> {:?} {}", initial_offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.tokens_collection.get_by_id(pointer_id) else {
                break;
            };

            if let Some(literal_text) = &pointer.literal {
                for (index, character) in literal_text.chars().enumerate() {
                    if is_first && index < token_offset {
                        continue;
                    }
                    // println!("CHAR: {} {}+{}", character, initial_offset, result.len());
                    result = format!("{}{}", result, character);
                    if needle_func(character, initial_offset + result.len()) {
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
        // println!("RESULT LENGTH: {} include_matched_char={include_matched_char}", result_length);

        if !is_done {
            result_length += 1;

            // No character ever matched!
            if !should_match_at_end {
                return Ok(None);
            }
        };

        let final_offset = initial_offset + (result_length-1);
        // println!("FINAL OFFSET: {final_offset}");

        if !include_matched_char && result_length > 0 {
            // println!("HERE?");
            // self.seek(final_offset-1);
            self.seek(final_offset);
            Ok(Some((
                initial_offset..final_offset,
                result[0..result_length-1].to_string(),
                SequentialTokenSelection::new(token_id, token_offset, result_length-1),
            )))
        } else {
            // self.seek(final_offset);
            self.seek(final_offset+1);
            println!("SEEK: {} => {}", initial_offset, final_offset);
            Ok(Some((
                initial_offset..final_offset+1,
                result,
                SequentialTokenSelection::new(token_id, token_offset, result_length),
            )))
        }
    }

    // Reads the document character by character in reverse from the current location to the start of
    // the tokens_collection until the specified `needle_func` passes, and then returns the data that has been read.
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
        SequentialTokenSelection, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        if *offset == 0 {
            // If already at the start, then there's nothing to match!
            return Ok(None);
        };
        let initial_offset = *offset - 1;
        let Some((token, mut token_offset)) = self.tokens_collection.get_by_offset(initial_offset) else {
            // If at the start of the document, the document may just be empty
            // So fail gracefully in this case
            if initial_offset == 0 {
                return Ok(None)
            }
            return Err(format!("Cannot get token at offset {} in tokens collection!", initial_offset));
        };
        let token_id = token.id;
        println!("TOK: {} -> {:?} {}", initial_offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.tokens_collection.get_by_id(pointer_id) else {
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
                    // println!("FOO {} {} - {}", character, initial_offset, result.len()-1);
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
                        SequentialTokenSelection::new_backwards(token_id, token_offset, result_length),
                    )))
                } else {
                    Ok(Some((
                        initial_offset+1..final_offset+1,
                        result[1..].to_string(),
                        SequentialTokenSelection::new_backwards(token_id, token_offset, result_length-1),
                    )))
                }
            } else {
                // One character might have been matched, but because `include_matched_char` is
                // false, this match results in being empty
                Ok(Some((
                    initial_offset+1..final_offset+1,
                    result[1..].to_string(),
                    SequentialTokenSelection::new_backwards(token_id, token_offset, 0),
                )))
            }
        } else {
            if include_matched_char {
                self.seek(final_offset);
                Ok(Some((
                    initial_offset+1..final_offset,
                    result,
                    SequentialTokenSelection::new_backwards(token_id, token_offset, result_length),
                )))
            } else {
                self.seek(final_offset);
                Ok(Some((
                    initial_offset+1..0,
                    result,
                    SequentialTokenSelection::new_backwards(token_id, token_offset, result_length),
                )))
            }
        }
    }

    pub fn read_to_pattern(
        &mut self,
        pattern: TraversalPattern,
        verb: &Option<Verb>,
        repeat_count: usize,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenSelection, /* The token range that was matched */
    )>, String> {
        let mut initial_offset = self.get_offset();
        let mut final_offset = initial_offset;

        let mut combined_result = String::from("");
        let mut combined_selection: Option<SequentialTokenSelection> = None;

        for index in 0..repeat_count {
            let is_not_last_iteration = index < repeat_count-1;
            println!("INDEX: {index}");
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
                    let result = self.read_forwards_until(|c, _| {
                        let is_current_word_char = is_word_char(c);

                        // Once whitespace is reached, continue matching until that whitespace ends
                        if hit_whitespace && started_in_word.is_none() && !is_whitespace_char(c) {
                            true
                        } else if started_in_word.is_none() && is_whitespace_char(c) {
                            hit_whitespace = true;
                            false

                        } else if let Some(started_in_word) = started_in_word {
                            println!("{started_in_word} != {is_current_word_char}");
                            // Keep matching characters until whether it is a word char or not
                            // changes
                            started_in_word != is_current_word_char
                        } else {
                            // Is the first character a word character or not?
                            started_in_word = Some(is_current_word_char);
                            false
                        }
                    }, false, true);

                    match result {
                        Ok(Some((range, literal, selection))) => {
                            // NOTE: the below is a bit of a special case. When navigating by word, add an
                            // extra character to the offset so that the cursor ends up right on top of the
                            // word instead of right before it
                            let mut modified_selection = selection.clone();
                            let mut seek_offset = 0;
                            if is_not_last_iteration || verb.is_none() || *verb == Some(Verb::Delete) {
                                modified_selection = modified_selection.select_whitespace_after(self)?;
                                if is_not_last_iteration || verb.is_none() {
                                    // Add one to the final seek position so the seek position ends
                                    // on the first char of the next word rather than the last char
                                    // of the previous word
                                    // modified_selection = modified_selection.add_to_end(1);
                                    // seek_offset += 1;
                                }
                            }
                            let char_count_change = modified_selection.char_count - selection.char_count;
                            self.seek(self.get_offset() + char_count_change + seek_offset);
                            println!("SELECTION? {modified_selection:?}");

                            Ok(Some((
                                modified_selection.range(self),
                                modified_selection.text(self),
                                modified_selection,
                            )))
                        },
                        other => other,
                    }
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
                    let mut first_char_was_whitespace = false;
                    let mut second_char_was_whitespace: Option<bool> = None;
                    let mut finished_whitespace = false;
                    let mut started_in_word: Option<bool> = None;

                    self.read_backwards_until(|c, _| {
                        // println!("CHAR: {c}");
                        if is_first {
                            first_char_was_other = is_other_char(c);
                            first_char_was_whitespace = is_whitespace_char(c);
                            is_first = false;
                            return false;
                        }
                        let is_current_word_char = is_word_char(c);

                        if first_char_was_whitespace && !is_whitespace_char(c) {
                            first_char_was_whitespace = false;
                        }

                        if first_char_was_other {
                            println!("IS OTHER {:?}", first_char_was_other);
                            !is_other_char(c)
                        } else if first_char_was_whitespace && is_whitespace_char(c) {
                            println!("IS WHITESPACE {:?}", first_char_was_whitespace);
                            false

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
                    let mut first_char_was_whitespace = false;
                    let mut second_char_was_whitespace: Option<bool> = None;
                    let mut finished_whitespace = false;

                    self.read_backwards_until(|c, _| {
                        if is_first {
                            is_first = false;
                            first_char_was_whitespace = is_whitespace_char(c); 
                            return false;
                        }
                        let is_current_not_whitespace_char = !is_whitespace_char(c);

                        if first_char_was_whitespace && !is_whitespace_char(c) {
                            first_char_was_whitespace = false;
                        }

                        if first_char_was_whitespace && is_whitespace_char(c) {
                            println!("IS WHITESPACE {:?}", first_char_was_whitespace);
                            false

                        // First, if there is immediately whitespace, scan through all of that
                        } else if second_char_was_whitespace.is_none() {
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

                    let result = self.read_forwards_until(|c, _| {
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
                    }, false, true);

                    match result {
                        Ok(Some((range, literal, selection))) => {
                            if is_not_last_iteration || verb.is_none() {
                                self.seek(self.get_offset() - 1);
                            }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::UpperEnd => {
                    if is_not_last_iteration || verb.is_none() {
                        self.seek(self.get_offset() + 1);
                    }

                    let mut finished_leading_space = false;
                    let result = self.read_forwards_until(|c, _| {
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
                    }, false, true);

                    match result {
                        Ok(Some((range, literal, selection))) => {
                            if is_not_last_iteration || verb.is_none() {
                                self.seek(self.get_offset() - 1);
                            }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::LowerBackEnd => {
                    let mut is_first = true;
                    let mut is_first_other_char = false;
                    let mut is_second = false;

                    let mut is_second_whitespace_char = false;
                    let mut is_second_word_char = false;
                    let mut is_second_other_char = false;

                    let mut started_whitespace = false;
                    let mut finished_whitespace = false;

                    self.seek(self.get_offset() + 1);
                    let result = self.read_backwards_until(|c, _| {
                        if is_first {
                            is_first = false;
                            if is_whitespace_char(c) {
                                started_whitespace = true;
                            }
                            is_first_other_char = is_other_char(c);
                            is_second = true;
                            return false;
                        }

                        if is_second {
                            is_second = false;
                            is_second_whitespace_char = is_whitespace_char(c);
                            if is_second_whitespace_char {
                                started_whitespace = true;
                            } else {
                                is_second_word_char = is_word_char(c);
                                is_second_other_char = is_other_char(c);
                                if !is_first_other_char && is_second_other_char {
                                    return true;
                                }
                                if is_first_other_char && !is_second_other_char {
                                    return true;
                                }
                            }
                        }

                        if is_whitespace_char(c) && !started_whitespace {
                            started_whitespace = true;
                            false

                        // To get to the previous word, follow a run of whitespace
                        } else if started_whitespace && !finished_whitespace {
                            if is_whitespace_char(c) {
                                false
                            } else {
                                true
                            }

                        // Stop matching once the type of character being matched changes
                        } else if is_second_word_char && !is_word_char(c) {
                            true
                        } else if is_second_other_char && !is_other_char(c) {
                            true
                        } else if is_second_whitespace_char && !is_whitespace_char(c) {
                            true

                        } else {
                            false
                        }
                    }, true, true);

                    match result {
                        Ok(Some((range, literal, mut selection))) => {
                            // FIXME: the below logic is a hack and may not be right
                            if let Some(mut result) = selection.move_backwards(self, 1) {
                                result.char_count -= 1;
                                selection = result;
                            } else {
                                panic!("TraversalPattern::LowerBackEnd selection could not be moved backwards!");
                            }
                            // if is_not_last_iteration || verb.is_none() {
                            //     self.seek(self.get_offset() - 1);
                            // }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::UpperBackEnd => {
                    let mut is_first = true;
                    let mut is_first_other_char = false;
                    let mut is_second = false;

                    let mut is_second_whitespace_char = false;
                    let mut is_second_word_char = false;
                    let mut is_second_other_char = false;

                    let mut started_whitespace = false;
                    let mut finished_whitespace = false;

                    self.seek(self.get_offset() + 1);
                    let result = self.read_backwards_until(|c, _| {
                        if is_first {
                            is_first = false;
                            if is_whitespace_char(c) {
                                started_whitespace = true;
                            }
                            is_second = true;
                            return false;
                        }

                        if is_second {
                            is_second = false;
                            is_second_whitespace_char = is_whitespace_char(c);
                            if is_second_whitespace_char {
                                started_whitespace = true;
                            } else {
                                is_second_word_char = is_word_char(c);
                                is_second_other_char = is_other_char(c);
                            }
                        }

                        if is_whitespace_char(c) && !started_whitespace {
                            started_whitespace = true;
                            false

                        // To get to the previous word, follow a run of whitespace
                        } else if started_whitespace && !finished_whitespace {
                            if is_whitespace_char(c) {
                                false
                            } else {
                                true
                            }

                        } else {
                            false
                        }
                    }, true, true);

                    match result {
                        Ok(Some((range, literal, mut selection))) => {
                            // FIXME: the below logic is a hack and may not be right
                            if let Some(mut result) = selection.move_backwards(self, 1) {
                                result.char_count -= 1;
                                selection = result;
                            } else {
                                panic!("TraversalPattern::UpperBackEnd selection could not be moved backwards!");
                            }
                            // if is_not_last_iteration || verb.is_none() {
                            //     self.seek(self.get_offset() - 1);
                            // }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::MatchingDelimiter => {
                    let initial_offset = self.get_offset();

                    #[derive(Debug)]
                    struct DelimeterSet {
                        search_forwards: bool,
                        backslash_escaping_supported: bool,

                        // Open delimeters are the series of chars that define the start
                        open_delimeter_list: Vec<&'static str>,

                        // Close delimeters are the series of chars that define the end
                        close_delimeter_list: Vec<&'static str>,

                        // End delimeters are a series of chars that one should stop on prematurely
                        // when searching FORWARDS through the document. Importantly, these
                        // sequences don't cause `depth` to be changed. This is primarily here
                        // for use with #if / #else / #endif constructs.
                        end_delimeter_list: Vec<&'static str>,
                    }

                    let original_is_backslash_escaped = self.read_forwards_and_backwards_to_find_backslashes() % 2 != 0;

                    // Figure out the start and end strings that represent an "open" and a "close"
                    //
                    // This is important so that matching patterns can be reference counted to
                    // determine which "open" goes with which "close"
                    let result = 'block: {
                        let start_parenthesis_match = self.read_if_matches("(")?;
                        let end_parenthesis_match = self.read_if_matches(")")?;
                        if start_parenthesis_match.is_some() || end_parenthesis_match.is_some() {
                            break 'block Some(DelimeterSet{
                                search_forwards: start_parenthesis_match.is_some(),
                                backslash_escaping_supported: true,
                                open_delimeter_list: vec!["("],
                                close_delimeter_list: vec![")"],
                                end_delimeter_list: vec![],
                            });
                        }

                        let start_square_match = self.read_if_matches("[")?;
                        let end_square_match = self.read_if_matches("]")?;
                        if start_square_match.is_some() || end_square_match.is_some() {
                            break 'block Some(DelimeterSet{
                                search_forwards: start_square_match.is_some(),
                                backslash_escaping_supported: true,
                                open_delimeter_list: vec!["["],
                                close_delimeter_list: vec!["]"],
                                end_delimeter_list: vec![],
                            });
                        }

                        let start_curly_match = self.read_if_matches("{")?;
                        let end_curly_match = self.read_if_matches("}")?;
                        if start_curly_match.is_some() || end_curly_match.is_some() {
                            break 'block Some(DelimeterSet{
                                search_forwards: start_curly_match.is_some(),
                                backslash_escaping_supported: true,
                                open_delimeter_list: vec!["{"],
                                close_delimeter_list: vec!["}"],
                                end_delimeter_list: vec![],
                            });
                        }

                        let c_block_comment_start = self.read_if_matches("/*")?;
                        let c_block_comment_end = self.read_if_matches("*/")?;
                        if c_block_comment_start.is_some() || c_block_comment_end.is_some() {
                            break 'block Some(DelimeterSet{
                                search_forwards: c_block_comment_start.is_some(),
                                backslash_escaping_supported: false,
                                open_delimeter_list: vec!["/*"],
                                close_delimeter_list: vec!["*/"],
                                end_delimeter_list: vec![],
                            });
                        }

                        let preprocesser_if = self.read_if_matches("#if")?;
                        let preprocesser_ifndef = self.read_if_matches("#ifndef")?;
                        let preprocesser_ifdef = self.read_if_matches("#ifdef")?;
                        let preprocesser_elif = self.read_if_matches("#elif")?;
                        let preprocesser_else = self.read_if_matches("#else")?;
                        if (
                            preprocesser_if.is_some() ||
                            preprocesser_ifndef.is_some() ||
                            preprocesser_ifdef.is_some() ||
                            preprocesser_elif.is_some() ||
                            preprocesser_else.is_some()
                        ) {
                            break 'block Some(DelimeterSet{
                                search_forwards: true,
                                backslash_escaping_supported: false,
                                open_delimeter_list: vec!["#if", "#ifndef", "#ifdef"],
                                close_delimeter_list: vec!["#endif"],
                                end_delimeter_list: vec!["#elif", "#else"],
                            });
                        }

                        let preprocesser_endif = self.read_if_matches("#endif")?;
                        if preprocesser_endif.is_some() {
                            break 'block Some(DelimeterSet{
                                search_forwards: false,
                                backslash_escaping_supported: false,
                                open_delimeter_list: vec!["#if", "#ifndef", "#ifdef"],
                                close_delimeter_list: vec!["#endif"],
                                end_delimeter_list: vec!["#elif", "#else"],
                            });
                        }

                        None
                    };
                    println!("MATCH DELIMITER: {:?} {}", result, original_is_backslash_escaped);

                    if let Some(DelimeterSet{
                        search_forwards,
                        backslash_escaping_supported,
                        open_delimeter_list,
                        close_delimeter_list,
                        end_delimeter_list,
                    }) = result {
                        // Add a backslash in front if the first token is escaped via an odd number
                        // of backslashes
                        let add_backslash_prefix = |n: &&str| {
                            if backslash_escaping_supported && original_is_backslash_escaped {
                                format!("\\{n}")
                            } else {
                                String::from(*n)
                            }
                        };
                        let open_delimeter_list_string: Vec<String> = open_delimeter_list
                            .iter()
                            .map(add_backslash_prefix)
                            .collect();
                        let close_delimeter_list_string: Vec<String> = close_delimeter_list
                            .iter()
                            .map(add_backslash_prefix)
                            .collect();
                        let end_delimeter_list_string: Vec<String> = end_delimeter_list
                            .iter()
                            .map(add_backslash_prefix)
                            .collect();

                        // Extract the first character of each type of delimeter for use in the
                        // search
                        let start_first_char_options: Vec<char> = open_delimeter_list_string.iter().map(
                            |start_delimeter| start_delimeter.chars().next().unwrap()
                        ).collect();
                        let close_first_char_options: Vec<char> = close_delimeter_list_string.iter().map(
                            |close_delimeter| close_delimeter.chars().next().unwrap()
                        ).collect();
                        let end_first_char_options: Vec<char> = end_delimeter_list_string.iter().map(
                            |end_delimeter| end_delimeter.chars().next().unwrap()
                        ).collect();

                        if !search_forwards {
                            self.seek(self.get_offset() - 1);
                        }

                        // Now, search for the other delimeter!
                        // NOTE: start at 1 because the first delimeter was just parsed above
                        // let mut depth = if search_forwards && open_delimeter_list.contains(&"#if") { 1 } else { 0 };
                        let mut depth = if search_forwards { 1 } else { 0 };
                        loop {
                            let read_result = if search_forwards {
                                self.read_forwards_until(
                                    |c, _| start_first_char_options.contains(&c) || close_first_char_options.contains(&c) || end_first_char_options.contains(&c),
                                    true,
                                    false,
                                )
                            } else {
                                self.read_backwards_until(
                                    |c, _| start_first_char_options.contains(&c) || close_first_char_options.contains(&c) || end_first_char_options.contains(&c),
                                    true,
                                    false,
                                )
                            }?;
                            println!("READ RESULT: {read_result:?}");

                            // Was the end of the document was reached with no match?
                            if read_result.is_none() {
                                break;
                            }
                            if let Some((_, _, selection)) = read_result {
                                // FIXME: this shouldn't be possible?
                                if selection.length_in_chars(self) == 0 {
                                    break;
                                }
                            }

                            // FIRST: look for open delimeters and increase the depth by one if it
                            // is found.
                            {
                                if search_forwards {
                                    self.seek_push(self.get_offset() - 1);
                                } else {
                                    self.seek_push(self.get_offset());
                                }
                                let start_is_backslash_escaped = self.read_forwards_and_backwards_to_find_backslashes() % 2 != 0;
                                let start_backslashes_match = if backslash_escaping_supported {
                                    start_is_backslash_escaped == original_is_backslash_escaped
                                } else { true };
                                let found_start_delimeter = open_delimeter_list_string.iter().find(|start_delimeter| {
                                    match self.read_if_matches(start_delimeter) {
                                        Ok(Some(_)) => true,
                                        _ => false,
                                    }
                                });
                                self.seek_pop();

                                if found_start_delimeter.is_some() && start_backslashes_match {
                                    println!("o---- {:?} {:?}", open_delimeter_list_string, found_start_delimeter);
                                    if search_forwards {
                                        depth += 1;
                                        println!("+1 -> depth={depth}");
                                    } else {
                                        depth -= 1;
                                        println!("-1 -> depth={depth}");
                                        if depth == 0 {
                                            // Found the matching delimeter!
                                            break;
                                        }
                                    }
                                    continue;
                                }
                            }

                            // SECOND: look for end delimeters, and if one is found at the top
                            // level, then end the match there.
                            {
                                if search_forwards {
                                    self.seek_push(self.get_offset() - 1);
                                } else {
                                    self.seek_push(self.get_offset());
                                }
                                let end_is_backslash_escaped = self.read_forwards_and_backwards_to_find_backslashes() % 2 != 0;
                                let end_backslashes_match = if backslash_escaping_supported {
                                    end_is_backslash_escaped == original_is_backslash_escaped
                                } else { true };
                                let found_end_delimeter = end_delimeter_list_string.iter().find(|end_delimeter| {
                                    match self.read_if_matches(end_delimeter) {
                                        Ok(Some(_)) => true,
                                        _ => false,
                                    }
                                });
                                self.seek_pop();
                                if found_end_delimeter.is_some() && end_backslashes_match {
                                    println!("e---- {:?} {:?}", end_delimeter_list_string, found_end_delimeter);
                                    if search_forwards {
                                        if depth == 1 {
                                            // Found the matching delimeter!
                                            break;
                                        }
                                    }
                                    continue;
                                }
                            }

                            // THIRD: look for close delimeters and decrease the depth by one if
                            // it is found.
                            {
                                if search_forwards {
                                    self.seek_push(self.get_offset() - 1);
                                } else {
                                    self.seek_push(self.get_offset());
                                }
                                let close_is_backslash_escaped = self.read_forwards_and_backwards_to_find_backslashes() % 2 != 0;
                                let close_backslashes_match = if backslash_escaping_supported {
                                    close_is_backslash_escaped == original_is_backslash_escaped
                                } else { true };
                                let found_close_delimeter = close_delimeter_list_string.iter().find(|close_delimeter| {
                                    match self.read_if_matches(close_delimeter) {
                                        Ok(Some(_)) => true,
                                        _ => false,
                                    }
                                });
                                self.seek_pop();
                                if found_close_delimeter.is_some() && close_backslashes_match {
                                    println!("c---- {:?} {:?}", close_delimeter_list_string, found_close_delimeter);
                                    if search_forwards {
                                        depth -= 1;
                                        println!("-1 -> depth={depth}");
                                        if depth == 0 {
                                            // Found the matching delimeter!
                                            break;
                                        }
                                    } else {
                                        depth += 1;
                                        println!("+1 -> depth={depth}");
                                    }
                                    continue;
                                }
                            }
                        }

                        let mut final_offset = self.get_offset();
                        if final_offset > 0 {
                            if is_not_last_iteration || verb.is_none() {
                                final_offset -= 1;
                            }
                            if !search_forwards {
                                final_offset += 1;
                                if !is_not_last_iteration && verb.is_some() {
                                    final_offset -= 2;
                                }
                            }
                        }

                        // If backslash escaping is supported, then the initial backslash was
                        // matched. The match should ACTUALLY be the first character.
                        if backslash_escaping_supported && original_is_backslash_escaped {
                            final_offset += 1;
                        }
                        self.seek(final_offset);
                        // println!("OFFSETS: {:?} {:?}", initial_offset, final_offset);

                        let mut selection = SequentialTokenSelection::new_from_offsets(
                            self,
                            initial_offset,
                            final_offset,
                        )?;
                        // println!("SELECTION: {:?}", selection);

                        if !search_forwards && final_offset == 0 {
                            selection.char_count += 1;
                            self.seek(0);
                        }

                        Ok(Some((
                            selection.range(self),
                            selection.text(self),
                            selection,
                        )))
                    } else {
                        Ok(None)
                    }
                },
                TraversalPattern::To(character) => {
                    let result = self.read_forwards_until(|c, _| c == character, false, false);
                    match result {
                        Ok(Some((range, literal, selection))) => {
                            if is_not_last_iteration || verb.is_none() {
                                self.seek(self.get_offset() - 1);
                            }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::UpperTo(character) => {
                    self.read_backwards_until(|c, _| c == character, false, false)
                },
                TraversalPattern::Find(character) => {
                    // If the current character the cursor is on matches the character to look for,
                    // then seek forward one character so that the current character doesn't match
                    let mut seek_backwards_again = false;
                    if let Ok(Some((_, next_character, _))) = self.read(1) {
                        if next_character != format!("{character}") {
                            seek_backwards_again = true;
                        }
                    }
                    if seek_backwards_again {
                        self.seek(self.get_offset() - 1);
                    }

                    let result = self.read_forwards_until(|c, _| c == character, true, false);
                    match result {
                        Ok(Some((range, literal, selection))) => {
                            if is_not_last_iteration || verb.is_none() {
                                self.seek(self.get_offset() - 1);
                            }

                            Ok(Some((range, literal, selection)))
                        },
                        other => other,
                    }
                },
                TraversalPattern::UpperFind(character) => {
                    // If the current character the cursor is on matches the character to look for,
                    // then seek forward one character so that the current character doesn't match
                    let mut seek_backwards = false;
                    self.seek(self.get_offset() - 1);
                    if let Ok(Some((_, current_character, _))) = self.read(1) {
                        if current_character == format!("{character}") {
                            seek_backwards = true;
                        }
                    }
                    if seek_backwards {
                        self.seek(self.get_offset() - 1);
                    }

                    self.read_backwards_until(|c, _| c == character, true, false)
                },
            };

            if hit_newline {
                break;
            }

            match result {
                Ok(Some((range, result, selection))) => {
                    final_offset = range.end;
                    println!("range={:?} result='{}' initial_offset={} final_offset={}", range, result, initial_offset, final_offset);

                    if final_offset > initial_offset {
                        combined_result = format!("{}{}", combined_result, result);
                    } else {
                        combined_result = format!("{}{}", result, combined_result);
                    }
                    if let Some(value) = combined_selection {
                        println!("{:?}.EXTEND({:?})", value, selection);
                        combined_selection = Some(value.extend(self, selection)?);
                    } else {
                        // println!("BACKWARDS: {:?}", selection);
                        // combined_selection = Some(selection.as_forwards_selection(self)?);
                        combined_selection = Some(selection);
                        // println!("FORWARDS: {:?}", combined_selection);
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

        if initial_offset == final_offset || combined_selection.is_none() {
            Ok(None)
        } else {
            Ok(Some((
                initial_offset..final_offset,
                combined_result,
                combined_selection.unwrap(),
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
        // Work from start_offset reading character by character through the tokens_collection until we are
        // at the right line

        // Then add `col_index` to get the offset

        let mut current_offset = start_offset;
        let mut current_row = start_row;
        self.seek_push(current_offset);
        while current_row < row {
            self.seek(current_offset);
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true, false) else {
                // There must not be an end of line for the rest of the text tokens_collection
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
                // There must not be an end of line for the rest of the text tokens_collection
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

    pub fn compute_offset_bounds_of_row_including_newline(&mut self, row: usize) -> Result<(usize, usize), String> {
        let initial_offset = self.convert_rows_cols_to_offset((row, 1));
        self.seek_push(initial_offset);
        let result = match self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true, true) {
            Ok(Some((range, _, _))) => Ok((range.start, range.end)),
            Ok(None) => Err(format!("Unable to find offset at end of row {}", row)),
            Err(err) => Err(err),
        };
        self.seek_pop();
        result
    }

    pub fn compute_length_of_row_in_chars_excluding_newline(&mut self, row: usize) -> Result<usize, String> {
        match self.compute_offset_bounds_of_row_including_newline(row) {
            Ok((initial_offset, final_offset)) => {
                let line_length_including_newline = final_offset - initial_offset;
                Ok(line_length_including_newline - 1)
            },
            Err(err) => Err(err),
        }
    }

    // Compute the total number of lines and characters in the document.
    // This function scans through all lines in the document, caching them as it goes, until it
    // gets to the end, and then it returns the total number of lines that it visited.
    //
    // NOTE: this can potentially be a very expensive calculation, because it can potentially loop
    // through all tokens in the document. However, repeated runs can be fast because newline
    // positions are cached.
    pub fn compute_max_number_of_rows_and_offset_of_last_row(&mut self) -> Result<(usize, usize), String> {
        self.seed_newline_cache_if_empty();

        let (final_cached_row, final_cached_row_offset) = self.newline_offset_cache
            .iter()
            .fold((1, 0), |(previous_row, previous_offset), (cached_row, cached_offset)| {
                if *cached_offset > previous_offset {
                    // Prefer the value that is later on in the list
                    (*cached_row, *cached_offset)
                } else {
                    (previous_row, previous_offset)
                }
            });

        let mut current_offset = final_cached_row_offset;
        let mut current_row = final_cached_row;
        self.seek_push(current_offset);
        loop {
            self.seek(current_offset);
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true, false) else {
                // There must not be an end of line for the rest of the text tokens_collection
                // So we are done
                break;
            };

            let number_of_matched_chars_including_newline = result.len();
            current_offset += number_of_matched_chars_including_newline;
            current_row += 1;

            self.newline_offset_cache.insert(current_row, current_offset);
        }
        self.seek_pop();

        Ok((current_row, current_offset))
    }

    // NOTE: see docs for `compute_max_number_of_rows_and_chars`
    pub fn compute_number_of_rows(&mut self) -> Result<usize, String> {
        let result = self.compute_max_number_of_rows_and_offset_of_last_row()?;
        Ok(result.0)
    }

    pub fn has_at_least_rows(&mut self, at_least_rows: usize) -> Result<bool, String> {
        // Because counting all rows is really slow, if there are cached newlines that are after
        // the row being queried, then assume that the document goes on beyond that point.
        let (final_cached_row, _) = self.newline_offset_cache
            .iter()
            .fold((1, 0), |(previous_row, previous_offset), (cached_row, cached_offset)| {
                if *cached_offset > previous_offset {
                    // Prefer the value that is later on in the list
                    (*cached_row, *cached_offset)
                } else {
                    (previous_row, previous_offset)
                }
            });

        if final_cached_row > at_least_rows {
            Ok(true)
        } else {
            let row_count = self.compute_number_of_rows()?;
            Ok(row_count >= at_least_rows)
        }
    }

    // NOTE: see docs for `compute_max_number_of_rows_and_chars`
    pub fn compute_max_offset(&mut self) -> Result<usize, String> {
        // Seek to the start of the last line
        let result = self.compute_max_number_of_rows_and_offset_of_last_row()?;
        let start_offset_of_last_row = result.1;
        self.seek_push(start_offset_of_last_row);

        // Seek to the end of the last line to get the final offset
        let result = match self.read_forwards_until(|_, _| false, false, true) {
            Ok(Some((_, _, selection))) => {
                Ok(start_offset_of_last_row + selection.length_in_chars(self) - 1)
            },
            Ok(None) => Ok(start_offset_of_last_row),
            Err(e) => Err(e),
        };
        self.seek_pop();
        result
    }

    pub fn has_at_least_offset(&mut self, at_least_offset: usize) -> Result<bool, String> {
        // Because counting all chars is really slow, if there are cached newlines that are after
        // the offset being queried, then assume that the document goes on beyond that point.
        let (_, final_cached_row_offset) = self.newline_offset_cache
            .iter()
            .fold((1, 0), |(previous_row, previous_offset), (cached_row, cached_offset)| {
                if *cached_offset > previous_offset {
                    // Prefer the value that is later on in the list
                    (*cached_row, *cached_offset)
                } else {
                    (previous_row, previous_offset)
                }
            });

        if final_cached_row_offset > at_least_offset {
            Ok(true)
        } else {
            let max_offset = self.compute_max_offset()?;
            Ok(max_offset >= at_least_offset)
        }
    }
}



// BufferOptions are used to track options that can be set or cleared which will effect how the
// editor works.
#[derive(Debug, Clone)]
struct BufferOptions {
    options: HashMap<String, String>,
}
impl BufferOptions {
    fn new_empty() -> Self {
        Self { options: HashMap::new() }
    }
    fn new_with_defaults() -> Self {
        let mut options = Self::new_empty();
        options.insert("matchpairs", "(:),{:},[:],<:>");
        options.insert("cpoptions", "aABceFs");
        options
    }

    pub fn insert(&mut self, key: &str, value: &str) {
        self.options.insert(String::from(key), String::from(value));
    }
    pub fn append(&mut self, key: &str, value: &str) {
        self.options.insert(String::from(key), format!("{}{}", self.get(key), value));
    }
    pub fn remove(&mut self, key: &str) {
        self.options.remove(key);
    }

    fn coerse_to_bool(raw_string: &str) -> bool {
        raw_string.to_ascii_lowercase().starts_with("t")
    }

    // Used by the % / MatchDelimeters command to figure out which delimeters should match.
    pub fn supported_match_delimeters(&self) -> Vec<(&str, &str, Option<&str>)> {
        self.get("matchpairs").split(",").flat_map(|open_close_end| {
            match open_close_end.split(":").collect::<Vec<&str>>()[..] {
                [open, close] => vec![(open, close, None)],
                [open, close, end] => vec![(open, close, Some(end))],
                [..] => vec![],
            }
        }).collect()
    }

    // Used by the % / MatchDelimeters command to figure out if prefixed backslash escapes before
    // `supported_match_delimeters` should only ever match with other escaped delimeters
    pub fn should_match_escaped_delimeters_distinctly(&self) -> bool {
        self.get("cpoptions").contains("M")
    }

    pub fn get(&self, key: &str) -> &str {
        match self.options.get(key) {
            Some(value) => value,
            None => "",
        }
    }
}
impl Index<&str> for BufferOptions {
    type Output = str;

    fn index(&self, key: &str) -> &str {
        self.get(key)
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
    LowerBackEnd,
    UpperBackEnd,
    MatchingDelimiter,
    To(char),
    UpperTo(char),
    Find(char),
    UpperFind(char),
    RepeatToFind,
    RepeatToFindBackwards,
    CurrentLine,
    NextLine,
    RestOfLine,
    StartOfLine,
    StartOfLineAfterIndentation,
    EndOfLine,
    GoToRow(usize),
    GoToFirstRow,
    GoToLastRow,
    GoToColumn(usize),
    GoToPercentage(usize),
    GoToByte(usize),

    // Inside / around specific nouns:
    Paragraph,
    Sentence,
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
pub struct Buffer {
    document: Box<Document>,
    mode: Mode,

    options: BufferOptions,

    // Position in the document in (rows, columns) format
    // (1, 1) is at the top left
    pub position: (usize, usize),

    // The column that when moving downwards the cursor would prefer to be in if possible
    //
    // Example: The cursor is at column 4, then moves down to the next row with 2 columns, then
    // moved down to a row with 10 columns. The cursor should go from 4 => 2 => 4.
    preferred_column: usize,

    // Command parsing state
    state: ViewState,
    command_count: String,
    is_backwards: bool,
    verb: Option<Verb>,
    noun: Option<Noun>,
    should_clear_command_count: bool,
    last_verb: Option<Verb>,
    last_noun: Option<Noun>,
    last_to_or_find: Option<Noun>,
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

impl Buffer {
    pub fn new(document: Box<Document>) -> Self {
        Self {
            document: document,
            mode: Mode::Normal,
            options: BufferOptions::new_with_defaults(),
            position: (1, 1),
            preferred_column: 1,
            state: ViewState::Initial,
            command_count: String::from(""),
            is_backwards: false,
            verb: None,
            noun: None,
            should_clear_command_count: false,
            last_verb: None,
            last_noun: None,
            last_to_or_find: None,
        }
    }
    fn clear_command(&mut self) {
        self.state = ViewState::Initial;
        self.command_count = String::from("");
        self.is_backwards = false;

        // Store the last character that was navigated to via t/T/f/F
        // This is so that ; and , can function
        match self.noun {
            Some(Noun::To(_)) | Some(Noun::UpperTo(_)) | Some(Noun::Find(_)) | Some(Noun::UpperFind(_)) => {
                self.last_to_or_find = self.noun.clone();
            }
            _ => {},
        }

        if self.verb.is_some() && self.noun.is_some() {
            // ref: https://stackoverflow.com/a/61127366
            self.last_verb = self.verb.take();
            self.last_noun = self.noun.take();
        }

        self.verb = None;
        self.noun = None;
    }
    pub fn reset(&mut self) {
        self.mode = Mode::Normal;
        self.clear_command();
    }

    pub fn dump(&self) -> ViewDumpedData {
        println!("-------------");
        println!("BUFFER STATE:");
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

    pub fn dump_string(&mut self) {
        let offset = self.document.convert_rows_cols_to_offset(self.position);
        let tokens_collection = self.document.tokens_mut();
        println!("---\n{}\n--- mode={:?} position={:?} offset={:?}", tokens_collection.debug_stringify_highlight(offset, offset+1), self.mode, self.position, offset);
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
            match character {
                // TODO:
                // h / j / k / l - moving around DONE
                // w / W / b / B / e / E - word+back+end DONE
                // ge / gE - go to end of previous word FIXME: write tests for this
                // 0 / ^ / $ - start + end of line DONE
                // g_ - last non blank char on line
                // f / F / t / T / ; / , - to+find DONE
                // dd / cc DONE
                // D / C / Y DONE
                // gg / G / 122G DONE
                // 10| - go to col number DONE
                // 50% - go to percentage of file DONE
                // 10go - go to `n`th byte in the file DONE
                // % - matching brace DONE FIXME: write tests for this
                // x/X - delete char DONE
                // r - replace char
                // ( / ) / { / } / [[ /  ]] - move back and forward sentences and paragraphs and sections, see below for
                // info on what these mean
                //
                // "+y - yank register
                // p / P / "+p - paste
                // ]p / [p - paste in current indentation level
                // a / A / i / I / o / O / s / S / C / R - insert mode stuff
                // <C-C> / ESC - back to normal mode
                // v / V / <C-V> - visual mode
                // . - repeat last
                // ma / dma / 'a / `a / g'a / g`a - marks
                // <C-U> / <C-D> - half page up / half page down
                // <C-B> / <C-F> - page up / down
                //
                // 2d3w should delete 6 words
                //
                // dvj / dVj / d<ctrl+v>j - make the action linewise / characterwise / blockwise
                //
                // ACTIONS:
                // |c|	c	change
                // |d|	d	delete
                // |y|	y	yank into register (does not change the text)
                // |~|	~	swap case (only if 'tildeop' is set)
                // |g~|	g~	swap case
                // |gu|	gu	make lowercase
                // |gU|	gU	make uppercase
                // |!|	!	filter through an external program
                // |=|	=	filter through 'equalprg' or C-indenting if empty
                // |gq|	gq	text formatting
                // |gw|	gw	text formatting with no cursor movement
                // |g?|	g?	ROT13 encoding
                // |>|	>	shift right
                // |<|	<	shift left
                // |zf|	zf	define a fold
                // |g@|	g@	call function set with the 'operatorfunc' option
                //
                // g0 / g^ / g$ - go to the first line of the wrapped line
                // gm / gM - related to wrapped lines, look it up with :h gm
                // gj / gk - related to wrapped lines, look it up with :h gj
                // - / + / _ - seems very similar to j/k? Look it up with :g +
                //
                // PERCENT MATCHING:
                //
                // %			Find the next item in this line after or under the
                //       cursor and jump to its match. |inclusive| motion.
                //       Items can be:
                //       ([{}])		parenthesis or (curly/square) brackets
                //           (this can be changed with the
                //           'matchpairs' option)
                //       /* */		start or end of C-style comment
                //       #if, #ifdef, #else, #elif, #endif
                //           C preprocessor conditionals (when the
                //           cursor is on the # or no ([{
                //           is following)
                //       For other items the matchit plugin can be used, see
                //       |matchit-install|.  This plugin also helps to skip
                //       matches in comments.
                //
                //       When 'cpoptions' contains "M" |cpo-M| backslashes
                //       before parens and braces are ignored.  Without "M" the
                //       number of backslashes matters: an even number doesn't
                //       match with an odd number.  Thus in "( \) )" and "\( (
                //       \)" the first and last parenthesis match.
                //
                //       When the '%' character is not present in 'cpoptions'
                //       |cpo-%|, parens and braces inside double quotes are
                //       ignored, unless the number of parens/braces in a line
                //       is uneven and this line and the previous one does not
                //       end in a backslash.  '(', '{', '[', ']', '}' and ')'
                //       are also ignored (parens and braces inside single
                //       quotes).  Note that this works fine for C, but not for
                //       Perl, where single quotes are used for strings.
                //
                //       Nothing special is done for matches in comments.  You
                //       can either use the matchit plugin |matchit-install| or
                //       put quotes around matches.
                //
                //       No count is allowed, {count}% jumps to a line {count}
                //       percentage down the file |N%|.  Using '%' on
                //       #if/#else/#endif makes the movement linewise.
                //
                // SENTENCES / PARAGRAPHS / SECTIONS:
                //
                //               *sentence*
                // A sentence is defined as ending at a '.', '!' or '?' followed by either the
                // end of a line, or by a space or tab.  Any number of closing ')', ']', '"'
                // and ''' characters may appear after the '.', '!' or '?' before the spaces,
                // tabs or end of line.  A paragraph and section boundary is also a sentence
                // boundary.
                // If the 'J' flag is present in 'cpoptions', at least two spaces have to
                // follow the punctuation mark; <Tab>s are not recognized as white space.
                // The definition of a sentence cannot be changed.
                //
                //               *paragraph*
                // A paragraph begins after each empty line, and also at each of a set of
                // paragraph macros, specified by the pairs of characters in the 'paragraphs'
                // option.  The default is "IPLPPPQPP TPHPLIPpLpItpplpipbp", which corresponds to
                // the macros ".IP", ".LP", etc.  (These are nroff macros, so the dot must be in
                // the first column).  A section boundary is also a paragraph boundary.
                // Note that a blank line (only containing white space) is NOT a paragraph
                // boundary.
                // Also note that this does not include a '{' or '}' in the first column.  When
                // the '{' flag is in 'cpoptions' then '{' in the first column is used as a
                // paragraph boundary |posix|.
                //
                //               *section*
                // A section begins after a form-feed (<C-L>) in the first column and at each of
                // a set of section macros, specified by the pairs of characters in the
                // 'sections' option.  The default is "SHNHH HUnhsh", which defines a section to
                // start at the nroff macros ".SH", ".NH", ".H", ".HU", ".nh" and ".sh".
                //
                // The "]]" and "[[" commands stop at the '{' in the first column.  This is
                // useful to find the start of a function in a C program.  To search for a '}' in
                // the first column, the end of a C function, use "][" (forward) or "[]"
                // (backward).  Note that the first character of the command determines the
                // search direction.
                //
                // If your '{' or '}' are not in the first column, and you would like to use "[["
                // and "]]" anyway, try these mappings: >
                //
                // Known issues:
                // - b doesnt work on leading whitespace
                // - f will find the same character that one is on currently as a match

                // When the noun "t"/"T"/"f"/"F" is used, the enxt character refers to the
                // character that should be navigated to.
                c if self.state == ViewState::PressedT => self.set_noun(Noun::To(c)),
                c if self.state == ViewState::PressedUpperT => self.set_noun(Noun::UpperTo(c)),
                c if self.state == ViewState::PressedF => self.set_noun(Noun::Find(c)),
                c if self.state == ViewState::PressedUpperF => self.set_noun(Noun::UpperFind(c)),
                ';' if self.last_to_or_find.is_some() => self.set_noun(Noun::RepeatToFind),
                ',' if self.last_to_or_find.is_some() => self.set_noun(Noun::RepeatToFindBackwards),

                // "Verbs"
                'd' if self.state == ViewState::Initial => self.set_verb(Verb::Delete),
                'y' if self.state == ViewState::Initial => self.set_verb(Verb::Yank),
                'c' if self.state == ViewState::Initial => self.set_verb(Verb::Change),
                // '>' if self.state == ViewState::Initial => self.set_verb(Verb::IndentRight),
                // '<' if self.state == ViewState::Initial => self.set_verb(Verb::IndentLeft),
                // '=' if self.state == ViewState::Initial => self.set_verb(Verb::AutoIndent),
                'U' if self.in_g_mode() => self.set_verb(Verb::Uppercase),
                'u' if self.in_g_mode() => self.set_verb(Verb::Lowercase),

                // Uppercase Verbs - ie, `D` / `C`
                'D' if self.state == ViewState::Initial => {
                    self.set_verb(Verb::Delete);
                    self.set_noun(Noun::RestOfLine);
                },
                'C' if self.state == ViewState::Initial => {
                    self.set_verb(Verb::Change);
                    self.set_noun(Noun::RestOfLine);
                },

                // "inside" and "around" - ie, `cip`
                'i' if self.state == ViewState::HasVerb => { self.state = ViewState::IsInside },
                'a' if self.state == ViewState::HasVerb => { self.state = ViewState::IsAround },

                // Repeated Verbs - ie, `cc`, `gUU`
                'c' | 'd' | 'y' if self.state == ViewState::HasVerb => self.set_noun(Noun::CurrentLine),
                'U' if self.state == ViewState::HasVerb && self.verb == Some(Verb::Uppercase) => self.set_noun(Noun::CurrentLine),
                'u' if self.state == ViewState::HasVerb && self.verb == Some(Verb::Lowercase) => self.set_noun(Noun::CurrentLine),


                // Noun-like objects that are only valid after `i`nside or `a`round
                //
                // NOTE: this must go before regular nouns, because there are things like `b` that
                // mean different things: `cib` is  "change inside block" BUT `cb`is "change back"
                'p' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::Paragraph);
                },
                's' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::Sentence)
                },
                '[' | ']' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockSquare)
                },
                '(' | ')' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockParenthesis)
                },
                '{' | '}' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockCurly)
                },
                '<' | '>' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockAngle)
                },
                'b' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockSquareOrParenthesis)
                },
                'B' if self.state == ViewState::IsInside || self.state == ViewState::IsAround => {
                    self.set_noun(Noun::BlockSquareOrCurly)
                },
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
                'w' => self.set_noun(Noun::LowerWord),
                'W' => self.set_noun(Noun::UpperWord),
                'b' => self.set_noun(Noun::LowerBack),
                'B' => self.set_noun(Noun::UpperBack),
                'e' if self.in_g_mode() => self.set_noun(Noun::LowerBackEnd),
                'E' if self.in_g_mode() => self.set_noun(Noun::UpperBackEnd),
                'e' => self.set_noun(Noun::LowerEnd),
                'E' => self.set_noun(Noun::UpperEnd),

                'h' => {
                    self.set_noun(Noun::Character);
                    self.is_backwards = true;
                },
                'j' => self.set_noun(Noun::NextLine),
                'k' => {
                    self.set_noun(Noun::NextLine);
                    self.is_backwards = true;
                },
                'l' => self.set_noun(Noun::Character),

                't' => self.state = ViewState::PressedT,
                'T' => self.state = ViewState::PressedUpperT,
                'f' => self.state = ViewState::PressedF,
                'F' => self.state = ViewState::PressedUpperF,

                // A number: adjust the number of times the command should be run
                '1'..='9' => {
                    if self.should_clear_command_count {
                        self.command_count = String::from("");
                    }
                    self.command_count = format!("{}{}", self.command_count, character);
                    self.should_clear_command_count = false;
                },
                '0' if !self.command_count.is_empty() => {
                    if self.should_clear_command_count {
                        self.command_count = String::from("");
                    }
                    self.command_count = format!("{}0", self.command_count);
                    self.should_clear_command_count = false;
                },

                '$' => self.set_noun(Noun::EndOfLine),
                '0' => self.set_noun(Noun::StartOfLine),
                '^' => self.set_noun(Noun::StartOfLineAfterIndentation),

                // 'gg' goes to the top
                'g' if self.in_g_mode() => self.set_noun(Noun::GoToFirstRow),
                'G' => {
                    match self.command_count.parse::<usize>() {
                        // 123G goes to a line
                        Ok(line_number) => self.set_noun(Noun::GoToRow(line_number)),
                        // G goes to the bottom
                        Err(_) => self.set_noun(Noun::GoToLastRow),
                    }
                },
                // `123|` goes to column 123
                '|' if !self.command_count.is_empty() => {
                    let col_number = self.command_count.parse::<usize>().unwrap();
                    self.set_noun(Noun::GoToColumn(col_number));
                },
                // `50%` goes to halfway through the document
                '%' if !self.command_count.is_empty() => {
                    let percentage = self.command_count.parse::<usize>().unwrap();
                    self.set_noun(Noun::GoToPercentage(percentage));
                },
                // `50go` goes to the 50th byte of the file
                'o' if !self.command_count.is_empty() && self.in_g_mode() => {
                    let byte_offset = self.command_count.parse::<usize>().unwrap();
                    self.set_noun(Noun::GoToByte(byte_offset));
                },

                '%' => self.set_noun(Noun::MatchingDelimiter),

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
                'X' if self.state == ViewState::Initial => {
                    self.set_verb(Verb::Delete);
                    self.set_noun(Noun::Character);
                    self.is_backwards = true;
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

    pub fn process_input(&mut self, input: &str) -> Result<bool, String> {
        let mut result = Ok(false);
        self.raw_parse_input(input, |inner_self| {
            // Once a command has completed processing, execute it!
            result = inner_self.execute_command();

            inner_self.clear_command();
        });
        result
    }

    fn execute_command(&mut self) -> Result<bool, String> {
        if self.state != ViewState::Complete {
            return Err(format!("Unable to run execute_command when self.state != ViewState::Complete (value was {:?})", self.state));
        }

        let mut skip_setting_preferred_column = false;

        let command_count = self.command_count.parse::<usize>().unwrap_or(1);
        let noun_match = match self.noun {
            Some(Noun::Character) => {
                if self.is_backwards {
                    self.document.read_to_pattern(TraversalPattern::Left, &self.verb, command_count)
                } else {
                    self.document.read_to_pattern(TraversalPattern::Right, &self.verb, command_count)
                }
            },
            Some(Noun::CurrentLine) | Some(Noun::RestOfLine) => {
                if self.is_backwards {
                    panic!("Noun::CurrentLine or Noun::RestOfLine cannot have self.is_backwards set!");
                }

                let mut initial_offset = self.document.get_offset();
                let (mut rows, mut cols) = self.document.convert_offset_to_rows_cols(initial_offset);
                let initial_rows = rows;
                let initial_cols = cols;

                // FIXME: make sure rows isn't too large and goes beyond the end of the document
                rows += command_count-1;

                match self.noun {
                    Some(Noun::CurrentLine) => {
                        // For repeated verbs (dd), start at the beginning of the line
                        initial_offset = self.document.convert_rows_cols_to_offset((initial_rows, 1));
                    },
                    Some(Noun::RestOfLine) => {
                        // For uppercase verbs (D), start at the char after the current position
                        initial_offset += 1;
                    },
                    _ => {},
                }
                let number_of_chars_in_next_row = self.document.compute_length_of_row_in_chars_excluding_newline(
                    rows,
                )?;
                cols = number_of_chars_in_next_row;
                println!("LINEWISE ROW COL: {:?} => {:?}", (initial_rows, initial_cols), (rows, cols));

                let mut final_offset = self.document.convert_rows_cols_to_offset((rows, cols));
                println!("LINEWISE OFFSETS: {:?} => {:?}", initial_offset, final_offset);
                self.document.seek(final_offset);

                if initial_offset == final_offset {
                    Ok(None)
                } else {
                    let mut selection = SequentialTokenSelection::new_from_offsets(
                        &mut self.document,
                        initial_offset,
                        final_offset,
                    )?;
                    if self.verb.is_some() {
                        selection = selection.add_to_end(1);
                        if selection.starting_token_offset > 0 {
                            if let Some(result) = selection.move_backwards(&mut self.document, 1) {
                                selection = result;
                            } else {
                                panic!("{:?} selection could not be moved backwards!", self.noun);
                            }
                        }

                        // FIXME: conditionally DO NOT run the below if on the last line:
                        selection.char_count += 1;
                    }
                    println!("END: {selection:?}");

                    Ok(Some((
                        selection.range(&mut self.document),
                        selection.text(&mut self.document),
                        selection,
                    )))
                }
            },
            Some(Noun::NextLine) => {
                let mut initial_offset = self.document.get_offset();
                let (mut rows, mut cols) = self.document.convert_offset_to_rows_cols(initial_offset);
                let initial_rows = rows;
                let initial_cols = cols;

                // NOTE: don't set the preferred column when moving to the next line because moving
                // up and down is the action that should USE the preferred line value to
                // dynamically change the column
                skip_setting_preferred_column = true;

                if self.is_backwards {
                    // FIXME: make sure rows isn't too small
                    // if rows == 0 {
                    //     Ok(None)
                    // }
                    rows -= command_count;
                } else {
                    // FIXME: make sure rows isn't too large
                    rows += command_count;
                }

                // FIXME:
                // If verb is defined, then move the initial and final selections to the start and
                // end of their respective rows. `is_backwards` will flip this!
                //
                // Also use this cached data to get the j/k staying in the same column even when
                // an intermediate line isn't long enough behavior working.
                if self.verb.is_some() {
                    if self.is_backwards {
                        let number_of_chars_in_initial_row = self.document.compute_length_of_row_in_chars_excluding_newline(
                            initial_rows,
                        )?;
                        initial_offset = self.document.convert_rows_cols_to_offset((initial_rows, number_of_chars_in_initial_row));
                        cols = 1;
                    } else {
                        initial_offset = self.document.convert_rows_cols_to_offset((initial_rows, 1));
                        let number_of_chars_in_next_row = self.document.compute_length_of_row_in_chars_excluding_newline(
                            rows,
                        )?;
                        cols = number_of_chars_in_next_row;
                    }
                }

                match self.document.compute_length_of_row_in_chars_excluding_newline(rows) {
                    Ok(row_length) => {
                        // Before generating the final row/col position, make sure that the desired
                        // position actually exists.
                        if cols > row_length {
                            cols = row_length;
                        } else if self.preferred_column > cols {
                            if self.preferred_column < row_length {
                                cols = self.preferred_column;
                            } else {
                                // Go to the end of the line, because self.preferred_column is
                                // further right than this column has characters
                                cols = row_length;
                            }
                        }

                        // Compute the final offset and generate the output selection!
                        println!("LINEWISE ROW COL: {:?} => {:?}", (initial_rows, initial_cols), (rows, cols));
                        let mut final_offset = self.document.convert_rows_cols_to_offset((rows, cols));
                        println!("LINEWISE OFFSETS: {:?} => {:?}", initial_offset, final_offset);
                        self.document.seek(final_offset);

                        if initial_offset == final_offset {
                            Ok(None)
                        } else {
                            let mut selection = SequentialTokenSelection::new_from_offsets(
                                &mut self.document,
                                initial_offset,
                                final_offset,
                            )?;
                            if self.verb.is_some() {
                                if !self.is_backwards {
                                    selection = selection.add_to_end(1);
                                }
                                selection = selection
                                    .select_whitespace_before(&mut self.document)?
                                    .select_whitespace_after(&mut self.document)?;
                            }
                            println!("END: {selection:?}");

                            Ok(Some((
                                selection.range(&mut self.document),
                                selection.text(&mut self.document),
                                selection,
                            )))
                        }
                    },
                    Err(e) => Err(e),
                }
            },
            Some(Noun::StartOfLine) => self.document.read_backwards_until(|c, _| c == NEWLINE_CHAR, false, true),
            Some(Noun::StartOfLineAfterIndentation) => {
                let initial_offset = self.document.get_offset();
                self.document.seek(initial_offset); // FIXME: I think this is redundant?

                // Go to the start of the line
                self.document.read_backwards_until(|c, _| c == NEWLINE_CHAR, false, true);

                // Then read forwards until the whitespace at the beginning stops
                match self.document.read_forwards_until(|c, _| !is_whitespace_char(c), false, true) {
                    Ok(Some((_, _, selection))) => {
                        // Use this final offset combined with the initial offset calculated at the
                        // very start to get the range
                        let final_offset = selection.compute_final_offset(&mut self.document);

                        let mut selection = SequentialTokenSelection::new_from_offsets(
                            &mut self.document,
                            initial_offset,
                            final_offset,
                        )?;
                        if selection.is_backwards {
                            if let Some(result) = selection.move_backwards(&mut self.document, 1) {
                                selection = result;
                            } else {
                                panic!("Noun::StartOfLineAfterIndentation selection could not be moved backwards!");
                            }
                        }

                        Ok(Some((
                            selection.range(&mut self.document),
                            selection.text(&mut self.document),
                            selection,
                        )))
                    },
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
                }
            },
            Some(Noun::EndOfLine) => {
                let result = self.document.read_forwards_until(|c, _| c == NEWLINE_CHAR, false, true);
                match result {
                    Ok(Some((range, literal, selection))) => {
                        self.document.seek(self.document.get_offset() - 1);

                        Ok(Some((range, literal, selection)))
                    },
                    other => other,
                }
            },
            Some(Noun::GoToRow(_)) | Some(Noun::GoToFirstRow) | Some(Noun::GoToLastRow) => {
                let initial_offset = self.document.get_offset();

                let line_number = match &self.noun {
                    Some(Noun::GoToRow(line_number)) => {
                        if *line_number == 0 {
                            panic!("Cannot process Noun::GoToRow(0) - 0 is an invalid line number!");
                        }
                        let can_go_to_line = self.document.has_at_least_rows(*line_number)?;
                        if can_go_to_line {
                            Ok(*line_number)
                        } else {
                            // If the row is too large, then go to the final row
                            self.document.compute_number_of_rows()
                        }
                    },
                    Some(Noun::GoToFirstRow) => Ok(1),
                    Some(Noun::GoToLastRow) => self.document.compute_number_of_rows(),
                    other => {
                        panic!("Unable to process noun {:?} as a line navigation event!", other);
                    },
                }?;

                // NOTE: if a line number is picked that is too large, the expected behavior is to
                // navigate to the final line in the document
                let offset = self.document.convert_rows_cols_to_offset((line_number, 1));
                self.document.seek_push(offset);

                // Then read forwards until the whitespace at the beginning stops
                match self.document.read_forwards_until(|c, _| !is_whitespace_char(c), false, true) {
                    Ok(Some((_, _, selection))) => {
                        // Use this final offset combined with the initial offset calculated at the
                        // very start to get the range
                        let final_offset = selection.compute_final_offset(&mut self.document);

                        let mut selection = SequentialTokenSelection::new_from_offsets(
                            &mut self.document,
                            initial_offset,
                            final_offset,
                        )?;

                        Ok(Some((
                            selection.range(&mut self.document),
                            selection.text(&mut self.document),
                            selection,
                        )))
                    },
                    Ok(None) => Ok(None),
                    Err(e) => Err(e),
                }
            },
            Some(Noun::GoToColumn(raw_column_number)) => {
                let initial_offset = self.document.get_offset();

                let (initial_rows, _) = self.document.convert_offset_to_rows_cols(initial_offset);
                match self.document.compute_length_of_row_in_chars_excluding_newline(initial_rows) {
                    Ok(row_length) => {
                        let column_number = if raw_column_number > row_length {
                            // NOTE: if a column number is picked that is too large, the expected behavior is to
                            // navigate to the final column in the line
                            row_length
                        } else {
                            raw_column_number
                        };

                        let final_offset = self.document.convert_rows_cols_to_offset((initial_rows, column_number));
                        self.document.seek(final_offset);

                        let mut selection = SequentialTokenSelection::new_from_offsets(
                            &mut self.document,
                            initial_offset,
                            final_offset,
                        )?;

                        Ok(Some((
                            selection.range(&mut self.document),
                            selection.text(&mut self.document),
                            selection,
                        )))
                    },
                    Err(e) => Err(e),
                }
            },
            Some(Noun::GoToPercentage(percentage)) => {
                if percentage == 0 {
                    panic!("Cannot execute Noun::GoToPercentage(0) - `0%` is an invalid syntax construction!");
                }
                if percentage > 100 {
                    panic!("Cannot execute Noun::GoToPercentage({percentage}) where {percentage} is > 100!");
                }
                let initial_offset = self.document.get_offset();

                let number_of_chars = self.document.compute_max_offset()?;
                // NOTE: the below math formula is from `:h N%` in the vim help pages
                let final_offset = (percentage * number_of_chars + 99) / 100;
                println!("FINAL: {number_of_chars} {final_offset}");


                self.document.seek(final_offset);

                let mut selection = SequentialTokenSelection::new_from_offsets(
                    &mut self.document,
                    initial_offset,
                    final_offset,
                )?;

                Ok(Some((
                    selection.range(&mut self.document),
                    selection.text(&mut self.document),
                    selection,
                )))
            },
            Some(Noun::GoToByte(byte_offset)) => {
                let initial_offset = self.document.get_offset();

                if byte_offset == 0 {
                    panic!("Cannot execute Noun::GoToByte(0) - byte offsets are 1 indexed!");
                }

                // FIXME: convert from bytes to character offsets in the document
                // At the moment some stuff is probably unintentially assuming that 1 byte = 1 char
                let mut final_offset = byte_offset - 1;

                // If the user tries to go to an offset after the end of the document, then
                // truncate to the start of the document
                let can_go_to_offset = self.document.has_at_least_offset(final_offset)?;
                if !can_go_to_offset {
                    final_offset = self.document.compute_max_offset()?;
                }

                self.document.seek(final_offset);

                let mut selection = SequentialTokenSelection::new_from_offsets(
                    &mut self.document,
                    initial_offset,
                    final_offset,
                )?;

                Ok(Some((
                    selection.range(&mut self.document),
                    selection.text(&mut self.document),
                    selection,
                )))
            },

            Some(Noun::LowerWord) => self.document.read_to_pattern(TraversalPattern::LowerWord, &self.verb, command_count),
            Some(Noun::UpperWord) => self.document.read_to_pattern(TraversalPattern::UpperWord, &self.verb, command_count),
            Some(Noun::LowerBack) => self.document.read_to_pattern(TraversalPattern::LowerBack, &self.verb, command_count),
            Some(Noun::UpperBack) => self.document.read_to_pattern(TraversalPattern::UpperBack, &self.verb, command_count),
            Some(Noun::LowerBackEnd) => self.document.read_to_pattern(TraversalPattern::LowerBackEnd, &self.verb, command_count),
            Some(Noun::UpperBackEnd) => self.document.read_to_pattern(TraversalPattern::UpperBackEnd, &self.verb, command_count),
            Some(Noun::MatchingDelimiter) => self.document.read_to_pattern(
                TraversalPattern::MatchingDelimiter,
                &self.verb,
                command_count,
            ),
            Some(Noun::LowerEnd) => self.document.read_to_pattern(TraversalPattern::LowerEnd, &self.verb, command_count),
            Some(Noun::UpperEnd) => self.document.read_to_pattern(TraversalPattern::UpperEnd, &self.verb, command_count),
            Some(Noun::To(c)) => self.document.read_to_pattern(TraversalPattern::To(c), &self.verb, command_count),
            Some(Noun::UpperTo(c)) => self.document.read_to_pattern(TraversalPattern::UpperTo(c), &self.verb, command_count),
            Some(Noun::Find(c)) => self.document.read_to_pattern(TraversalPattern::Find(c), &self.verb, command_count),
            Some(Noun::UpperFind(c)) => self.document.read_to_pattern(TraversalPattern::UpperFind(c), &self.verb, command_count),
            Some(Noun::RepeatToFind) if self.last_to_or_find.is_some() => {
                let noun_traversal = match self.last_to_or_find {
                    Some(Noun::To(c)) => Ok(TraversalPattern::To(c)),
                    Some(Noun::Find(c)) => Ok(TraversalPattern::Find(c)),
                    Some(Noun::UpperTo(c)) => Ok(TraversalPattern::UpperTo(c)),
                    Some(Noun::UpperFind(c)) => Ok(TraversalPattern::UpperFind(c)),
                    _ => Err(format!("Cannot compute traversal for noun {:?}", self.last_to_or_find)),
                }?;

                self.document.read_to_pattern(noun_traversal, &self.verb, command_count)
            },
            Some(Noun::RepeatToFindBackwards) if self.last_to_or_find.is_some() => {
                let inverted_noun_traversal = match self.last_to_or_find {
                    Some(Noun::To(c)) => Ok(TraversalPattern::UpperTo(c)),
                    Some(Noun::Find(c)) => Ok(TraversalPattern::UpperFind(c)),
                    Some(Noun::UpperTo(c)) => Ok(TraversalPattern::To(c)),
                    Some(Noun::UpperFind(c)) => Ok(TraversalPattern::Find(c)),
                    _ => Err(format!("Cannot inverted traversal for noun {:?}", self.last_to_or_find)),
                }?;

                self.document.read_to_pattern(
                    inverted_noun_traversal,
                    &self.verb,
                    command_count,
                )
            },

            // Some(Noun::Inside(n)) | Some(Noun::Around(n)) if n == Noun::Paragraph {
            //     let mut last_char = ' ';
            //
            //     let is_inside = if let Some(Noun::Inside(_n)) = self.Noun {
            //         true
            //     } else {
            //         false
            //     };
            //
            //     let initial_offset = self.document.get_offset();
            //
            //     let (_, _, backwards_selection) = self.read_backwards_until(|c, _| {
            //     });
            //
            //     self.seek(initial_offset);
            //
            //     // Read from the current position up to a double newline
            //     let (_, _, forwards_selection) = self.read_forwards_until(|c, _| {
            //         if last_char == NEWLINE_CHAR && c == NEWLINE_CHAR {
            //             true
            //         } else {
            //             last_char = c;
            //             false
            //         }
            //     }, is_around, true);
            // },
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
            _ => self.document.read(1),
        };

        let Some((_, _, selection)) = noun_match.unwrap() else {
            // Nothing matched the given noun
            return Ok(false);
        };

        let verb_result = match self.verb {
            Some(Verb::Delete) => {
                let deleted_selection = selection.remove_deep(&mut self.document, false).unwrap();

                // After the delete, reset the offset to the start of the deletion operation
                let tokens_collection = self.document.tokens_mut();
                let new_offset = deleted_selection.compute_start_offset(&mut self.document);

                // If the character this offset is supposed to be on is greater than the number of
                // characters in that row, then move the selection to the end of the row.
                //
                // An example case where this is hit is with `D`.
                let (rows, cols) = self.document.convert_offset_to_rows_cols(new_offset);
                let modified_new_offset = match self.document.compute_length_of_row_in_chars_excluding_newline(rows) {
                    Ok(row_length) => {
                        if cols > row_length {
                            Ok(self.document.convert_rows_cols_to_offset((rows, row_length)))
                        } else {
                            Ok(new_offset)
                        }
                    },
                    Err(e) => Err(e),
                }?;

                self.document.seek(modified_new_offset);
                Ok(())
            },
            // Yank,
            // Change,
            // IndentRight,
            // IndentLeft,
            // AutoIndent,
            // SwapCase,
            // Uppercase,
            // Lowercase,

            _ => Ok(()),
        };

        if let Err(e) = verb_result {
            return Err(e);
        };

        // After changing the character, adjust the preferred character if the user has moved
        // further to the right.
        //
        // This ensures that moving straight down with (for example) `jjjjj` always is in the same
        // column.
        if !skip_setting_preferred_column {
            let final_offset = self.document.get_offset();
            let (_, end_cols) = self.document.convert_offset_to_rows_cols(final_offset);
            self.preferred_column = end_cols;
        }

        // Update the newline cache to expire entries including and after the selection
        let minimum_extent_of_selection = selection.minimum_offset_extent(&mut self.document);
        self.document.clear_newline_cache_at(minimum_extent_of_selection);

        // Update the cursor to be in the right spot
        let offset = self.document.get_offset();
        println!("FINAL OFFSET: {offset}");
        self.position = self.document.convert_offset_to_rows_cols(offset);

        Ok(true)
    }
}


#[cfg(test)]
mod test_engine {
    use regex::Regex;
    use super::*;

    fn remove_sequentialtokenrange(v: Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenSelection, /* The token range that was matched */
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

    mod test_document {
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
        fn it_is_able_to_read_from_document_one_at_a_time() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            // Get a few subranges to make sure they generate the right data
            let mut document = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            document.seek(0);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((0..3, String::from("111")))));

            let mut document = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            document.seek(1);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((1..4, String::from("112")))));

            let mut document = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            document.seek(2);
            assert_eq!(remove_sequentialtokenrange(document.read(2)), Ok(Some((2..4, String::from("12")))));

            // Make sure that if at the end, as much data is returned as possible
            let mut document = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            document.seek(3);
            assert_eq!(remove_sequentialtokenrange(document.read(5)), Ok(Some((3..4, String::from("2")))));
        }

        #[test]
        fn it_is_able_to_read_from_document_repeatedly() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
            assert_eq!(result.0, TokenParseStatus::FullParse); // status

            // Make sure the parser parsed the input that was expected
            assert_eq!(result.4.stringify(), "1112");

            // Get a few subranges to make sure they generate the right data
            let mut document = Document::new_from_tokenscollection(Box::new(result.4));
            document.seek(0);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((0..3, String::from("111")))));
            document.seek(1);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((1..4, String::from("112")))));
            document.seek(2);
            assert_eq!(remove_sequentialtokenrange(document.read(2)), Ok(Some((2..4, String::from("12")))));

            // Make sure that if at the end, as much data is returned as possible
            document.seek(3);
            assert_eq!(remove_sequentialtokenrange(document.read(5)), Ok(Some((3..4, String::from("2")))));
        }

        #[test]
        fn it_is_able_to_read_from_plain_text_document_repeatedly() {
            // Get a few subranges to make sure they generate the right data
            let mut document = Document::new_from_literal("foo bar baz");
            document.seek(0);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((0..3, String::from("foo")))));
            document.seek(1);
            assert_eq!(remove_sequentialtokenrange(document.read(3)), Ok(Some((1..4, String::from("oo ")))));
            document.seek(2);
            assert_eq!(remove_sequentialtokenrange(document.read(5)), Ok(Some((2..7, String::from("o bar")))));
            document.seek(6);
            assert_eq!(remove_sequentialtokenrange(document.read(4)), Ok(Some((6..10, String::from("r ba")))));

            // Make sure that if at the end, as much data is returned as possible
            document.seek(9);
            assert_eq!(remove_sequentialtokenrange(document.read(10)), Ok(Some((9..11, String::from("az")))));
        }

        mod read_forwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == 'b', true, false)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(document.get_offset(), 5);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == 'b', false, false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(document.get_offset(), 4);
            }

            #[test]
            fn it_should_seek_by_index_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|_, i| i >= 5, true, false)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(document.get_offset(), 5);
            }

            #[test]
            fn it_should_seek_by_index_not_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|_, i| i >= 5, false, false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(document.get_offset(), 4);
            }

            #[test]
            fn it_should_never_match_a_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == 'X', false, false)),
                    Ok(None)
                );
                assert_eq!(document.get_offset(), 0);
            }

            #[test]
            fn it_should_seek_forward_in_sequence() {
                let mut document = Document::new_from_literal("foo bar baz");

                // First seek to the first space
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == ' ', true, false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(document.get_offset(), 4);

                // Then seek to right before the `a`
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == 'a', false, false)),
                    Ok(Some((4..5, "b".to_string())))
                );
                assert_eq!(document.get_offset(), 5);

                // Then seek by index most of the way to the end
                assert_eq!(
                    remove_sequentialtokenrange(document.read_forwards_until(|_, i| i >= 10, true, false)),
                    Ok(Some((5..10, "ar ba".to_string())))
                );
                assert_eq!(document.get_offset(), 10);
            }
        }

        mod read_backwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                document.seek(10);
                assert_eq!(
                    remove_sequentialtokenrange(document.read_backwards_until(|c, _| c == 'r', true, false)),
                    Ok(Some((10..6, "r ba".to_string())))
                );
                assert_eq!(document.get_offset(), 6);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut document = Document::new_from_literal("foo bar baz");
                document.seek(10);
                assert_eq!(
                    remove_sequentialtokenrange(document.read_backwards_until(|c, _| c == 'r', false, false)),
                    Ok(Some((10..7, " ba".to_string())))
                );
                assert_eq!(document.get_offset(), 7);
            }
        }

        #[test]
        fn it_should_seek_forward_and_backwards_in_sequence() {
            let mut document = Document::new_from_literal("foo bar baz");

            // First seek to the first space
            assert_eq!(
                remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == ' ', true, false)),
                Ok(Some((0..4, "foo ".to_string())))
            );
            assert_eq!(document.get_offset(), 4);

            // Then seek back a few characters
            assert_eq!(
                remove_sequentialtokenrange(document.read_backwards_until(|c, _| c == 'f', true, false)),
                Ok(Some((4..0, "foo ".to_string())))
            );
            assert_eq!(document.get_offset(), 0);

            // Then seek to the first space, NOT INCLUDING IT
            assert_eq!(
                remove_sequentialtokenrange(document.read_forwards_until(|c, _| c == ' ', false, false)),
                Ok(Some((0..3, "foo".to_string())))
            );
            assert_eq!(document.get_offset(), 3);

            // FIXME: this is not working
            // // Then seek back a few characters again, NOT INCLUDING IT
            // assert_eq!(
            //     remove_sequentialtokenrange(document.read_backwards_until(|c, _| c == 'f', false, false)),
            //     Ok(Some((3..1, "oo".to_string())))
            // );
            // assert_eq!(document.get_offset(), 1);
        }

        mod read_to_pattern {
            use super::*;

            mod lower_word {
                use super::*;

                #[test]
                fn it_should_change_lower_word_then_whitespace_at_start() {
                    let mut document = Document::new_from_literal("foo bar baz");

                    // Get the first lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_lower_word_at_start() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");

                    // Get the first lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), ".foo bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TEST.foo bar baz");
                }

                #[test]
                fn it_should_change_lower_word_in_middle() {
                    let mut document = Document::new_from_literal("foo.foo bar   baz");
                    document.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..11);
                    assert_eq!(matched_chars, "bar");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo    baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST   baz");
                }

                #[test]
                fn it_should_change_lower_word_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 12);
                    assert_eq!(modified_selection.char_count, 3);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_change_2_lower_words_in_middle_right_up_to_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
                        2,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..15);
                    assert_eq!(matched_chars, "bar baz");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 7);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_lower_words_at_end_and_run_out_of_chars() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_lower_words_at_end_and_spill_over_to_next_line() {
                    let mut document = Document::new_from_literal("foo.foo bar baz\nqux quux");
                    document.seek(8); // Move to the start of "bar"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
                        3, // NOTE: this spills over to the next line!
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..19);
                    assert_eq!(matched_chars, "bar baz\nqux");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 11);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST quux");
                }

                #[test]
                fn it_should_change_lower_words_at_end_of_string_ending_with_whitespace() {
                    let mut document = Document::new_from_literal("foo.foo bar baz     ");
                    document.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar      ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TEST     ");
                }
            }

            mod upper_word {
                use super::*;

                #[test]
                fn it_should_change_upper_word_then_whitespace_at_start() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");

                    // Get the first upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..8);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 8);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_word_at_start() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");

                    // Get the first upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 0..8);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 8);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 0);
                    assert_eq!(modified_selection.char_count, 7);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new()).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_word_with_punctuation_and_spaces_after_in_middle() {
                    let mut document = Document::new_from_literal("foo.foo bar..b_ar   baz");
                    document.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..20);
                    assert_eq!(matched_chars, "bar..b_ar   ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 12);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 9);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo    baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST   baz");
                }

                #[test]
                fn it_should_change_upper_word_with_numbers_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz1!");
                    document.seek(12); // Move to the start of "baz"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 12);
                    assert_eq!(modified_selection.char_count, 5);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_change_2_upper_words_in_middle_right_up_to_end() {
                    let mut document = Document::new_from_literal("foo.foo bar.bar baz.baz quux");
                    document.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 15);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST quux");
                }

                #[test]
                fn it_should_change_3_upper_words_at_end_and_run_out_of_chars() {
                    let mut document = Document::new_from_literal("foo.foo bar.bar baz.baz");
                    document.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
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
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 15);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST");
                }

                #[test]
                fn it_should_change_3_upper_words_at_end_and_spill_over_to_next_line() {
                    let mut document = Document::new_from_literal("foo.foo bar baz.baz\nqux.qux quux");
                    document.seek(8); // Move to the start of "bar"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperWord,
                        &Some(Verb::Change),
                        3, // NOTE: this spills over to the next line!
                    ).unwrap().unwrap();
                    // assert_eq!(range, 8..28);
                    assert_eq!(matched_chars, "bar baz.baz\nqux.qux ");
                    assert_eq!(selection.starting_token_offset, 8);
                    assert_eq!(selection.char_count, 20);

                    // When performing a CHANGE, remove whitespace after the token prior to executing
                    // the operation
                    let modified_selection = selection.unselect_whitespace_after(&mut document).unwrap();
                    assert_eq!(modified_selection.starting_token_offset, 8);
                    assert_eq!(modified_selection.char_count, 19);

                    // Delete it
                    let deleted_selection = modified_selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo  quux");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo TEST quux");
                }
            }

            mod lower_back {
                use super::*;

                #[test]
                fn it_should_change_lower_back_at_start() {
                    let mut document = Document::new_from_literal("foo bar baz");
                    document.seek(4); // Move to the start of "bar"

                    // Get the first lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 4..0);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 3);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_in_middle() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(8); // Move to the start of "bar"

                    // Go back a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..4);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 7);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_in_middle_with_whitespace() {
                    let mut document = Document::new_from_literal("foo.foo      bar baz");
                    document.seek(13); // Move to the start of "bar"

                    // Go back a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 13..4);
                    assert_eq!(matched_chars, "foo      ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 9);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(14); // Move to the end of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 14..12);
                    assert_eq!(matched_chars, "ba");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 13);
                    assert_eq!(selection.char_count, 2);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar z");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TESTz");
                }

                #[test]
                fn it_should_seek_repeatedly() {
                    // First space          ----> "TEST bar.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(3);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 3..0);
                        assert_eq!(matched_chars, "foo");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 2);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), " bar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "TEST bar.baaaaar baz");
                    }

                    // First char of "bar"  ----> "TESTbar.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(4);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 4..0);
                        assert_eq!(matched_chars, "foo ");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 3);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "bar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "TESTbar.baaaaar baz");
                    }

                    // Second char of "bar"  ----> "foo TESTar.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(5);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 5..4);
                        assert_eq!(matched_chars, "b");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo ar.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo TESTar.baaaaar baz");
                    }

                    // Third char of "bar"  -> "foo TESTr.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(6);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 6..4);
                        assert_eq!(matched_chars, "ba");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 5);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo r.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo TESTr.baaaaar baz");
                    }

                    // Period               -> "foo TEST.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(7);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 7..4);
                        assert_eq!(matched_chars, "bar");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 6);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo .baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo TEST.baaaaar baz");
                    }

                    // First char of "baaa" -> "foo barTESTbaaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(8);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 8..7);
                        assert_eq!(matched_chars, ".");
                        assert_eq!(selection.starting_token_offset, 7);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo barbaaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo barTESTbaaaaar baz");
                    }

                    // Second char of "baaa" > "foo bar.TESTaaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(9);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 9..8);
                        assert_eq!(matched_chars, "b");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 1);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.aaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.TESTaaaaar baz");
                    }

                    // Third char of "baaa" > "foo bar.TESTaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(10);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 10..8);
                        assert_eq!(matched_chars, "ba");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 9);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.aaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.TESTaaaar baz");
                    }

                    // Space after "baaaar" > "foo bar.TEST baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(15);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerBack,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 15..8);
                        assert_eq!(matched_chars, "baaaaar");
                        assert_eq!(selection.is_backwards, true);
                        assert_eq!(selection.starting_token_offset, 14);
                        assert_eq!(selection.char_count, 7);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar. baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.TEST baz");
                    }
                }
            }

            mod upper_back {
                use super::*;

                #[test]
                fn it_should_change_upper_back_at_start() {
                    let mut document = Document::new_from_literal("foo bar baz");
                    document.seek(4); // Move to the start of "bar"

                    // Go back an upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 4..0);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 3);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_in_middle() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(8); // Move to the start of "bar"

                    // Go back an upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 8..0);
                    assert_eq!(matched_chars, "foo.foo ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 7);
                    assert_eq!(selection.char_count, 8);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_upper_back_in_middle_with_whitespace() {
                    let mut document = Document::new_from_literal("foo foo      bar baz");
                    document.seek(13); // Move to the start of "bar"

                    // Go back a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 13..4);
                    assert_eq!(matched_chars, "foo      ");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 9);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo TESTbar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(14); // Move to the end of "baz"

                    // Go back an upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperBack,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 14..12);
                    assert_eq!(matched_chars, "ba");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 13);
                    assert_eq!(selection.char_count, 2);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar z");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TESTz");
                }
            }

            mod lower_end {
                use super::*;

                #[test]
                fn it_should_change_lower_end_at_start() {
                    let mut document = Document::new_from_literal("foo bar baz");

                    // Go to the end of the first word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_lower_end_in_middle() {
                    let mut document = Document::new_from_literal("foo bar.bar baz");
                    document.seek(4); // Move to the start of "bar.bar"

                    // Get the end of the second word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 4..7);
                    assert_eq!(matched_chars, "bar");
                    assert_eq!(selection.starting_token_offset, 4);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo .bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo TEST.bar baz");
                }

                #[test]
                fn it_should_change_lower_back_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(12); // Move to the start of "baz"

                    // Get a lower word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::LowerEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TEST");
                }

                #[test]
                fn it_should_seek_repeatedly() {
                    // First space          ----> "fooTEST.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(3);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 3..7);
                        assert_eq!(matched_chars, " bar");
                        assert_eq!(selection.starting_token_offset, 3);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "fooTEST.baaaaar baz");
                    }

                    // First char of "bar"  ----> "foo TEST.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(4);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 4..7);
                        assert_eq!(matched_chars, "bar");
                        assert_eq!(selection.starting_token_offset, 4);
                        assert_eq!(selection.char_count, 3);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo .baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo TEST.baaaaar baz");
                    }

                    // Second char of "bar"  ----> "foo TESTar.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(5);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 5..7);
                        assert_eq!(matched_chars, "ar");
                        assert_eq!(selection.starting_token_offset, 5);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo b.baaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bTEST.baaaaar baz");
                    }

                    // Third char of "bar"  -> "foo baTESTbaaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(6);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 6..8);
                        assert_eq!(matched_chars, "r.");
                        assert_eq!(selection.starting_token_offset, 6);
                        assert_eq!(selection.char_count, 2);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo babaaaaar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo baTESTbaaaaar baz");
                    }

                    // Period               -> "foo TEST.baaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(7);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 7..15);
                        assert_eq!(matched_chars, ".baaaaar");
                        assert_eq!(selection.starting_token_offset, 7);
                        assert_eq!(selection.char_count, 8);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo barTEST baz");
                    }

                    // First char of "baaa" -> "foo barTESTbaaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(8);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 8..15);
                        assert_eq!(matched_chars, "baaaaar");
                        assert_eq!(selection.starting_token_offset, 8);
                        assert_eq!(selection.char_count, 7);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar. baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.TEST baz");
                    }

                    // Second char of "baaa" > "foo bar.TESTaaaaar baz"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(9);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 9..15);
                        assert_eq!(matched_chars, "aaaaar");
                        assert_eq!(selection.starting_token_offset, 9);
                        assert_eq!(selection.char_count, 6);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.b baz");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.bTEST baz");
                    }

                    // Space after "baaaar" > "foo bar.baaaaarTEST"
                    {
                        let mut document = Document::new_from_literal("foo bar.baaaaar baz");
                        document.seek(15);
                        let (range, matched_chars, selection) = document.read_to_pattern(
                            TraversalPattern::LowerEnd,
                            &Some(Verb::Change),
                            1,
                        ).unwrap().unwrap();
                        assert_eq!(range, 15..19);
                        assert_eq!(matched_chars, " baz");
                        assert_eq!(selection.starting_token_offset, 15);
                        assert_eq!(selection.char_count, 4);

                        // Delete it
                        let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.baaaaar");

                        // Replace it with TEST
                        deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                        assert_eq!(document.tokens_mut().stringify(), "foo bar.baaaaarTEST");
                    }
                }
            }

            mod upper_end {
                use super::*;

                #[test]
                fn it_should_change_upper_end_at_start() {
                    let mut document = Document::new_from_literal("foo bar baz");

                    // Go to the end of the first word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..3);
                    assert_eq!(matched_chars, "foo");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), " bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TEST bar baz");
                }

                #[test]
                fn it_should_change_upper_end_in_middle() {
                    let mut document = Document::new_from_literal("foo bar.bar baz");
                    document.seek(4); // Move to the start of "bar.bar"

                    // Get the end of the second word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 4..11);
                    assert_eq!(matched_chars, "bar.bar");
                    assert_eq!(selection.starting_token_offset, 4);
                    assert_eq!(selection.char_count, 7);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo  baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo TEST baz");
                }

                #[test]
                fn it_should_change_upper_back_at_end() {
                    let mut document = Document::new_from_literal("foo.foo bar baz");
                    document.seek(12); // Move to the start of "baz"

                    // Get a upper word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperEnd,
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    assert_eq!(range, 12..15);
                    assert_eq!(matched_chars, "baz");
                    assert_eq!(selection.starting_token_offset, 12);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar ");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foo.foo bar TEST");
                }
            }

            mod to_and_find {
                use super::*;

                #[test]
                fn it_should_change_to_char() {
                    let mut document = Document::new_from_literal("foo bar baz");

                    // Go to the end of the first word
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::To('b'),
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..4);
                    assert_eq!(matched_chars, "foo ");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "bar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TESTbar baz");
                }

                #[test]
                fn it_should_change_backwards_to_char() {
                    let mut document = Document::new_from_literal("foo bar baz");
                    document.seek(6); // Seek to the end of "bar"

                    // Go to the previous 'o'
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperTo('o'),
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 6..3);
                    assert_eq!(matched_chars, " ba");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 5);
                    assert_eq!(selection.char_count, 3);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "foor baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "fooTESTr baz");
                }

                #[test]
                fn it_should_find_char_and_change() {
                    let mut document = Document::new_from_literal("foo bar baz");

                    // Find the next 'b'
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::Find('b'),
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 0..5);
                    assert_eq!(matched_chars, "foo b");
                    assert_eq!(selection.starting_token_offset, 0);
                    assert_eq!(selection.char_count, 5);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "ar baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "TESTar baz");
                }

                #[test]
                fn it_should_find_char_backwards_and_change() {
                    let mut document = Document::new_from_literal("foo bar baz");
                    document.seek(6); // Seek to the end of "bar"

                    // Find the previous 'o'
                    let (range, matched_chars, selection) = document.read_to_pattern(
                        TraversalPattern::UpperFind('o'),
                        &Some(Verb::Change),
                        1,
                    ).unwrap().unwrap();
                    // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                    assert_eq!(range, 6..2);
                    assert_eq!(matched_chars, "o ba");
                    assert_eq!(selection.is_backwards, true);
                    assert_eq!(selection.starting_token_offset, 5);
                    assert_eq!(selection.char_count, 4);

                    // Delete it
                    let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                    assert_eq!(document.tokens_mut().stringify(), "for baz");

                    // Replace it with TEST
                    deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                    assert_eq!(document.tokens_mut().stringify(), "foTESTr baz");
                }

                // #[test]
                // fn it_should_find_char_and_repeat() {
                //     let mut document = Document::new_from_literal("foo bar baz");
                //
                //     // Find the next 'b'
                //     let (range, matched_chars, selection) = document.read_to_pattern(
                //         TraversalPattern::Find('b'),
                //         &None,
                //         1,
                //     ).unwrap().unwrap();
                //     assert_eq!(range, 0..5);
                //     assert_eq!(matched_chars, "foo b");
                //     assert_eq!(selection.starting_token_offset, 0);
                //     assert_eq!(selection.char_count, 5);
                //
                //     // Repeat the find again
                //     let (range, matched_chars, selection) = document.read_to_pattern(
                //         TraversalPattern::Find('b'),
                //         &Some(Verb::Change),
                //         1,
                //     ).unwrap().unwrap();
                //     // println!("RESULT: {:?} '{}' {:?}", range, matched_chars, selection);
                //     assert_eq!(range, 4..9);
                //     assert_eq!(matched_chars, "ar b");
                //     assert_eq!(selection.starting_token_offset, 5);
                //     assert_eq!(selection.char_count, 4);
                //
                //     // Delete it
                //     let deleted_selection = selection.remove_deep(&mut document, true).unwrap();
                //     assert_eq!(document.tokens_mut().stringify(), "foo az");
                //
                //     // Replace it with TEST
                //     deleted_selection.prepend_text(&mut document, String::from("TEST"), &HashMap::new());
                //     assert_eq!(document.tokens_mut().stringify(), "foTESTaz");
                // }
            }
        }
    }

    mod test_buffer {
        use super::*;

        #[test]
        fn it_should_parse_many_different_sequences() {
            let mut document = Document::new_from_literal("foo.foo bar baz");
            let mut buffer = document.create_buffer();

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
                ("cib", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Inside(Box::new(Noun::BlockSquareOrParenthesis))),
                }),
                ("ciB", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Inside(Box::new(Noun::BlockSquareOrCurly))),
                }),

                // Linewise operations
                ("dd", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Delete),
                    noun: Some(Noun::CurrentLine),
                }),
                ("3cc", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 3,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::CurrentLine),
                }),
                ("5guu", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 5,
                    verb: Some(Verb::Lowercase),
                    noun: Some(Noun::CurrentLine),
                }),
                ("gUU", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Uppercase),
                    noun: Some(Noun::CurrentLine),
                }),
                ("12gUU", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 12,
                    verb: Some(Verb::Uppercase),
                    noun: Some(Noun::CurrentLine),
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
                buffer.raw_parse_input(input_text, |_| {});
                assert_eq!(buffer.dump(), dumped_data, "Assertion failed: `{}`", input_text);
                buffer.reset();
            }
        }

        mod test_up_down_left_right {
            use super::*;

            #[test]
            fn it_should_navigate_around() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("lljjhkdl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbrbar\nbazbaz");
            }

            #[test]
            fn it_should_preserve_the_active_column_when_moving_through_a_shorter_row() {
                let mut document = Document::new_from_literal("oneone\ntwo\nthreethree");
                let mut buffer = document.create_buffer();

                buffer.process_input("$");
                buffer.process_input("jj");
                buffer.process_input("x");
                assert_eq!(buffer.document.tokens_mut().stringify(), "oneone\ntwo\nthreehree");
            }

            #[test]
            fn it_should_delete_left() {
                let mut document = Document::new_from_literal("foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("edh");
                assert_eq!(buffer.document.tokens_mut().stringify(), "fo.foo bar baz");
            }

            #[test]
            fn it_should_delete_right() {
                let mut document = Document::new_from_literal("foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "oo.foo bar baz");
            }

            #[test]
            fn it_should_delete_up() {
                let mut document = Document::new_from_literal("foo\nbar\nbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("jdk");
                assert_eq!(buffer.document.tokens_mut().stringify(), "baz");
            }

            #[test]
            fn it_should_delete_3_up() {
                let mut document = Document::new_from_literal("one\ntwo\nthree\nfour\nfive");
                let mut buffer = document.create_buffer();

                buffer.process_input("jjjd3k");
                assert_eq!(buffer.document.tokens_mut().stringify(), "five");
            }

            #[test]
            fn it_should_delete_down() {
                let mut document = Document::new_from_literal("foo\nbar\nbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("dj");
                assert_eq!(buffer.document.tokens_mut().stringify(), "baz");
            }

            #[test]
            fn it_should_delete_2_down() {
                let mut document = Document::new_from_literal("foo\nbar\nbaz\nquux");
                let mut buffer = document.create_buffer();

                buffer.process_input("d2j");
                assert_eq!(buffer.document.tokens_mut().stringify(), "quux");
            }

            #[test]
            fn it_should_delete_all_lines() {
                let mut document = Document::new_from_literal("foo\nbar");
                let mut buffer = document.create_buffer();

                buffer.process_input("dj");
                assert_eq!(buffer.document.tokens_mut().stringify(), "");
            }
        }

        mod test_start_end_of_line {
            use super::*;

            #[test]
            fn it_should_delete_to_start_of_line() {
                let mut document = Document::new_from_literal("foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("lld0");
                assert_eq!(buffer.document.tokens_mut().stringify(), "o.foo bar baz");
            }

            #[test]
            fn it_should_delete_to_end_of_line() {
                let mut document = Document::new_from_literal("foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("lld$");
                assert_eq!(buffer.document.tokens_mut().stringify(), "fo");
            }

            #[test]
            fn it_should_delete_to_whitespace_sensitive_start_of_line() {
                let mut document = Document::new_from_literal("    foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("d^");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foo.foo bar baz");
            }

            #[test]
            fn it_should_delete_to_whitespace_sensitive_start_of_line_starting_in_middle() {
                let mut document = Document::new_from_literal("    foo.foo bar baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("fbd^");
                assert_eq!(buffer.document.tokens_mut().stringify(), "    bar baz");
            }
        }

        mod test_linewise {
            use super::*;

            #[test]
            fn it_should_run_linewise_delete() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("dd");
                assert_eq!(buffer.document.tokens_mut().stringify(), "barbar\nbazbaz");
            }

            #[test]
            fn it_should_run_linewise_delete_in_middle() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("jdd");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbazbaz");
            }

            #[test]
            fn it_should_run_linewise_delete_multi_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("2dd");
                assert_eq!(buffer.document.tokens_mut().stringify(), "bazbaz");
            }

            #[test]
            fn it_should_run_linewise_delete_all_lines() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("3dd");
                assert_eq!(buffer.document.tokens_mut().stringify(), "");
            }
        }

        mod test_rest_of_line {
            use super::*;

            #[test]
            fn it_should_run_delete_to_end_of_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("3lD");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_run_delete_to_end_of_line_and_next_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("3l2D");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foo\nbazbaz");
            }

            #[test]
            fn it_should_run_delete_to_end_of_line_on_last_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("jj3lD");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nbaz");
            }
        }

        mod test_go_to_line {
            use super::*;

            #[test]
            fn it_should_go_to_line_1() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                // Go to not the first character of the line
                buffer.process_input("lll");
                // Go to line 1, delete a char
                buffer.process_input("1Gdl");
                // Make sure the first character of line 1 is deleted
                assert_eq!(buffer.document.tokens_mut().stringify(), "oofoo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_line_2() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("2Gdl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\narbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_line_2_with_starting_whitespace() {
                let mut document = Document::new_from_literal("foofoo\n  barbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("2Gdl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\n  arbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_line_999() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("999Gdl");
                // Going to line after the end of the document should go to the last line
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nazbaz");
            }

            #[test]
            fn it_should_go_to_first_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                // Go to not the first character of the line
                buffer.process_input("lll");
                // Go to first line, delete a char
                buffer.process_input("ggdl");
                // Make sure the first character of the first line is deleted
                assert_eq!(buffer.document.tokens_mut().stringify(), "oofoo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_last_line() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("GGdl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nazbaz");
            }
        }

        mod test_go_to_column {
            use super::*;

            #[test]
            fn it_should_go_to_column_3() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("3|dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "fofoo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_column_999() {
                let mut document = Document::new_from_literal("foofao\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("999|dl");
                // Since 999 is beyond the length of the row, delete the last char in the row
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofa\nbarbar\nbazbaz");
            }
        }

        mod test_go_to_percentage {
            use super::*;

            #[test]
            fn it_should_go_to_50_percent() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("50%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_100_percent() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("100%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nbazba");
            }

            #[test]
            fn it_should_go_to_1_percent() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("1%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "fofoo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_be_unable_to_go_to_0_percent() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                // Go to NOT the first line and not the first char
                buffer.process_input("jjll");

                // Try to go to 0% and delete a char
                // NOTE that `0%` will be interpreted as `0` `%`, so it will go to the start
                // of the line
                buffer.process_input("0%dl");

                // Make sure that going to 0% did not go to the start of the document
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nazbaz");
            }
        }

        mod test_go_to_byte_offset {
            use super::*;

            #[test]
            fn it_should_go_to_byte_offset_4() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("4godl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foooo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_byte_offset_999() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                buffer.process_input("999godl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foofoo\nbarbar\nbazba");
            }

            #[test]
            #[ignore]
            fn it_should_go_to_byte_offset_0() {
                // FIXME: this test doesn't pass yet
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                // Go to NOT the first line and not the first char
                buffer.process_input("jjll");

                buffer.process_input("0godl");
                // Make sure that the first character was removed
                // NOTE: 0 and 1 should do the same thing
                assert_eq!(buffer.document.tokens_mut().stringify(), "oofoo\nbarbar\nbazbaz");
            }

            #[test]
            fn it_should_go_to_byte_offset_1() {
                let mut document = Document::new_from_literal("foofoo\nbarbar\nbazbaz");
                let mut buffer = document.create_buffer();

                // Go to NOT the first line and not the first char
                buffer.process_input("jjll");

                buffer.process_input("1godl");
                // Make sure that the first character was removed
                assert_eq!(buffer.document.tokens_mut().stringify(), "oofoo\nbarbar\nbazbaz");
            }
        }
        mod test_percent_match_delimeter {
            use super::*;

            #[test]
            fn it_should_go_to_other_parenthesis() {
                let mut document = Document::new_from_literal("(foo bar) baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "(foo bar baz");
            }

            #[test]
            fn it_should_go_to_other_nested_parenthesis() {
                let mut document = Document::new_from_literal("(foo (bar) baz)");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "(foo (bar) baz");
            }

            #[test]
            fn it_should_go_to_escaped_parenthesis() {
                let mut document = Document::new_from_literal("\\(foo (bar\\) baz)");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "\\(foo (bar\\ baz)");
            }

            #[test]
            fn it_should_go_to_other_curly_brace() {
                let mut document = Document::new_from_literal("{foo bar} baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "{foo bar baz");
            }

            #[test]
            fn it_should_go_to_other_nested_curly_brace() {
                let mut document = Document::new_from_literal("{foo {bar} baz}");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "{foo {bar} baz");
            }

            #[test]
            fn it_should_go_to_escaped_curly_brace() {
                let mut document = Document::new_from_literal("\\{foo {bar\\} baz}");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "\\{foo {bar\\ baz}");
            }

            #[test]
            fn it_should_go_to_other_square_bracket() {
                let mut document = Document::new_from_literal("[foo bar] baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "[foo bar baz");
            }

            #[test]
            fn it_should_go_to_other_nested_square_bracket() {
                let mut document = Document::new_from_literal("[foo [bar] baz]");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "[foo [bar] baz");
            }

            #[test]
            fn it_should_go_to_escaped_square_bracket() {
                let mut document = Document::new_from_literal("\\[foo [bar\\] baz]");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "\\[foo [bar\\ baz]");
            }

            #[test]
            fn it_should_navigate_with_many_types_of_brackets() {
                let mut document = Document::new_from_literal("[foo (b[ar] {baz])}");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "[foo (b[ar] {baz)}");
            }

            #[test]
            fn it_should_not_navigate_with_unmatched_brackets() {
                let mut document = Document::new_from_literal("[foo (bar) baz}");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                assert_eq!(buffer.document.tokens_mut().stringify(), "foo (bar) baz}");
            }

            #[test]
            fn it_should_go_to_other_c_multiline_comment() {
                let mut document = Document::new_from_literal("/*foo bar*/ baz");
                let mut buffer = document.create_buffer();

                buffer.process_input("%dl");
                // FIXME: this test isn't quite right, the expected output is `/*foo bar* baz`
                assert_eq!(buffer.document.tokens_mut().stringify(), "/*foo bar/ baz");
            }

            #[test]
            fn it_should_go_through_all_parts_of_c_preprocessor_if() {
                let raw_document = "#if foo\n  one\n#elif bar\n  two\n#else\n  three\n#endif";

                // Part one: #if -> #elif
                {
                    let mut document = Document::new_from_literal(raw_document);
                    let mut buffer = document.create_buffer();

                    buffer.process_input("%dl");
                    assert_eq!(
                        buffer.document.tokens_mut().stringify(),
                        "#if foo\n  one\nelif bar\n  two\n#else\n  three\n#endif"
                    );
                }

                // Part two: #if -> #elif -> #else
                {
                    let mut document = Document::new_from_literal(raw_document);
                    let mut buffer = document.create_buffer();
                    buffer.process_input("%");
                    buffer.process_input("%dl");
                    assert_eq!(
                        buffer.document.tokens_mut().stringify(),
                        "#if foo\n  one\n#elif bar\n  two\nelse\n  three\n#endif"
                    );
                }

                // Part three: #if -> #elif -> #else -> #endif
                {
                    let mut document = Document::new_from_literal(raw_document);
                    let mut buffer = document.create_buffer();
                    buffer.process_input("%");
                    buffer.process_input("%");
                    buffer.process_input("%dl");
                    assert_eq!(
                        buffer.document.tokens_mut().stringify(),
                        "#if foo\n  one\n#elif bar\n  two\n#else\n  three\nendif"
                    );
                }

                // Part four: #if -> #elif -> #else -> #endif -> #if
                {
                    let mut document = Document::new_from_literal(raw_document);
                    let mut buffer = document.create_buffer();
                    buffer.process_input("%");
                    buffer.process_input("%");
                    buffer.process_input("%");
                    buffer.process_input("%dl");
                    assert_eq!(
                        buffer.document.tokens_mut().stringify(),
                        "if foo\n  one\n#elif bar\n  two\n#else\n  three\n#endif"
                    );
                }
            }

            #[test]
            fn it_should_respect_nested_c_preprocessor_ifs() {
                let mut document = Document::new_from_literal("#if foo\n  one\n#else\n  #if bar\n  two\n  #endif\n#endif");
                let mut buffer = document.create_buffer();

                buffer.process_input("%"); // #if -> #else
                buffer.process_input("%dl"); // #else -> second #endif
                assert_eq!(
                    buffer.document.tokens_mut().stringify(),
                    "#if foo\n  one\n#else\n  #if bar\n  two\n  #endif\nendif"
                );
            }
        }
    }
}
