use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;

use crate::token::*;
use crate::token_match_template::*;

const NEWLINE_CHAR: char = '\n';

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
pub struct SequentialTokenRange {
    starting_token_id: uuid::Uuid,
    starting_token_offset: usize,
    is_backwards: bool,
    char_count: usize,
}
impl SequentialTokenRange {
    pub fn new(
        starting_token_id: uuid::Uuid,
        starting_token_offset: usize,
        char_count: usize
    ) -> SequentialTokenRange {
        SequentialTokenRange {
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
    ) -> SequentialTokenRange {
        SequentialTokenRange {
            starting_token_id,
            starting_token_offset,
            is_backwards: true,
            char_count,
        }
    }

    // Remove all tokens within the token range. If `keep_first_token` is true, then the first
    // token is kept in the token list but left blank, likely because the user execured a change
    // rather than a delete and this initial token is where the change text will end up
    pub fn remove_deep(
        &self,
        tokens_collection: &mut Box<TokensCollection>,
        keep_first_token: bool,
    ) -> Result<(), String> {
        let mut chars_removed = 0;
        let mut is_first = true;
        let mut pointer_id = self.starting_token_id;
        let mut token_ids_to_remove = vec![];

        loop {
            let result = {
                let Some(pointer) = tokens_collection.get_by_id(pointer_id) else {
                    return Err(format!("Unable to find token with id {} ({} chars in to removal)", pointer_id, chars_removed));
                };

                // If moving backwards, then select the right field to move to next:
                let subsequent_id = if self.is_backwards {
                    pointer.previous_id
                } else {
                    pointer.next_id
                };

                Ok((subsequent_id, pointer.literal.clone()))
            };

            match result {
                Ok((pointer_subsequent_id, pointer_literal)) => {
                    if let Some(literal_text) = pointer_literal {
                        let literal_text_length = literal_text.len();
                        chars_removed += literal_text_length;

                        if chars_removed <= self.char_count {
                            if is_first {
                                if self.starting_token_offset > 0 {
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
                            if chars_removed == self.char_count {
                                break;
                            }
                        } else {
                            chars_removed -= literal_text_length;
                            // Part of this token needs to stay around - also, this is the last token
                            let number_of_chars_to_keep = self.char_count - chars_removed;
                            tokens_collection.get_by_id_mut(pointer_id, |pointer| {
                                pointer.literal = Some(String::from(&literal_text[number_of_chars_to_keep..]));
                            });
                            break;
                        }
                    };

                    if let Some(subsequent_id) = pointer_subsequent_id {
                        pointer_id = subsequent_id;
                    } else {
                        // Reached the end of the token stream
                        return Err(format!("Unable to remove SequentialTokenRange: ran out of tokens (start at {} offset {} and went {} of {} chars)", self.starting_token_id, self.starting_token_offset, chars_removed, self.char_count));
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

        Ok(())
    }

    pub fn replace_text(
        &self,
        tokens_collection: &mut Box<TokensCollection>,
        new_text: String,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<Option<uuid::Uuid>, String> {
        // Remove all other tokens in the sequence EXCEPT for the first one:
        self.remove_deep(tokens_collection, false)?;

        let mut complete_new_text = new_text;
        if let Some(starting_token) = tokens_collection.get_by_id(self.starting_token_id) {
            if let Some(literal_text) = &starting_token.literal {
                complete_new_text = String::from(format!(
                    "{}{}",
                    &literal_text[..self.starting_token_offset],
                    complete_new_text, /* this is equal to `new_text` here */
                ));
                if literal_text.len() > self.starting_token_offset + self.char_count {
                    complete_new_text = String::from(format!(
                        "{}{}",
                        complete_new_text,
                        literal_text,
                    ));
                }
            }
        };

        println!("NEW: {} {}", self.starting_token_offset, complete_new_text);

        // Then change the first one to have the new token text:
        tokens_collection.change_token_literal_text(
            self.starting_token_id,
            complete_new_text,
            token_match_templates_map,
        )
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
    pub fn new_from_tokenscollection(document: Box<TokensCollection>) -> Buffer {
        Buffer {
            document: document,
            offset_stack: vec![0],
            newline_offset_cache: HashMap::new(),
        }
    }
    pub fn new_from_literal(literal: &str) -> Buffer {
        Self::new_from_tokenscollection(
            Box::new(TokensCollection::new_unparsed_literal(literal)),
        )
    }

    pub fn create_view(self) -> View {
        View::new(Box::new(self))
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
    pub fn read(&mut self, number_of_chars: usize) -> Result<String, String> {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let token_id = token.id.clone();

        let result = self.document.stringify_for_offset(token_id, token_offset+number_of_chars);
        if result.len() >= token_offset+number_of_chars {
            self.seek(offset + number_of_chars);
            return Ok(String::from(&result[token_offset..token_offset+number_of_chars]));
        } else {
            // The buffer isn't long enough to return the full length of data, so return what we
            // can
            let chars = &result[token_offset..];
            self.seek(offset + chars.len());
            return Ok(String::from(chars));
        }
    }

    // Reads the buffer character by character until the passed `needle_func` passes, and then
    // returns the data that has been read.
    //
    // If `include_matched_char` is true, the final matched character is included, otherwise it is
    // not.
    pub fn read_forwards_until<F>(
        &mut self,
        mut needle_func: F,
        include_matched_char: bool,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        let token_id = token.id;
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(mut pointer) = self.document.get_by_id(pointer_id) else {
                break;
            };

            if let Some(literal_text) = &pointer.literal {
                for (index, character) in literal_text.chars().enumerate() {
                    if is_first && index < token_offset {
                        continue;
                    }
                    println!("CHAR: {}", character);
                    result = format!("{}{}", result, character);
                    if needle_func(character, offset + (result.len()-1)) {
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

        if !is_done {
            // No character ever matched!
            return Ok(None);
        };

        let result_length = result.len();
        if !include_matched_char && result_length > 0 {
            let initial_offset = *offset;
            let final_offset = initial_offset + (result_length-1);
            self.seek(final_offset);
            // println!("SEEK: {} => {}", initial_offset, final_offset);
            Ok(Some((
                initial_offset..final_offset,
                result[0..result_length-1].to_string(),
                SequentialTokenRange::new(token_id, token_offset, result_length-1),
            )))
        } else {
            let initial_offset = *offset;
            let final_offset = initial_offset + (result_length-1);
            self.seek(final_offset);
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
    pub fn read_backwards_until<F>(
        &mut self,
        mut needle_func: F,
        include_matched_char: bool,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
        SequentialTokenRange, /* The token range that was matched */
    )>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        let token_id = token.id;
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token_id;
        loop {
            let Some(mut pointer) = self.document.get_by_id(pointer_id) else {
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
                    println!("FOO {} {} - {}", character, offset, result.len()-1);
                    if needle_func(character, offset - (result.len() - 1)) {
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

        if !is_done {
            // No character ever matched!
            return Ok(None);
        };

        let result_length = result.len();

        if !include_matched_char && result.len() > 0 {
            let initial_offset = *offset;
            let final_offset = initial_offset - (result_length-1);
            self.seek(final_offset); // FIXME: I think this is wrong
            Ok(Some((
                initial_offset+1..final_offset+1,
                result[1..].to_string(),
                SequentialTokenRange::new_backwards(token_id, token_offset, result_length-1),
            )))
        } else {
            let initial_offset = *offset;
            let final_offset = initial_offset - (result_length-1);
            self.seek(final_offset);
            Ok(Some((
                initial_offset+1..final_offset,
                result,
                SequentialTokenRange::new_backwards(token_id, token_offset, result_length),
            )))
        }
    }

    pub fn read_to_pattern(
        &mut self,
        pattern: TraversalPattern,
        repeat_count: usize,
    ) -> Result<Option<(
        std::ops::Range<usize>, /* The matched token offset range */
        String, /* The matched literal data in all tokens, concatenated */
    )>, String> {
        let initial_offset = self.get_offset();
        let mut final_offset = initial_offset;

        let mut combined_result = String::from("");

        for _index in 0..repeat_count {
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
                    }, true)
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
                    }, true)
                },
                TraversalPattern::LowerWord => {
                    let mut hit_whitespace = false;
                    self.read_forwards_until(|c, _| {
                        if c == '\n' {
                            hit_newline = true;
                            true
                        } else if c.is_ascii_lowercase() || c.is_ascii_uppercase() {
                            hit_whitespace
                        } else {
                            hit_whitespace = true;
                            false
                        }
                    }, true)
                },
                TraversalPattern::UpperWord => {
                    let mut hit_whitespace = false;
                    self.read_forwards_until(|c, _| {
                        if c == '\n' {
                            hit_newline = true;
                            true
                        } else if !c.is_ascii_whitespace() {
                            hit_whitespace
                        } else {
                            hit_whitespace = true;
                            false
                        }
                    }, true)
                },
                TraversalPattern::LowerBack => {
                    self.read_backwards_until(|c, _| {
                        if c.is_ascii_lowercase() || c.is_ascii_uppercase() {
                            false
                        } else {
                            true
                        }
                    }, true)
                },
                TraversalPattern::UpperBack => {
                    self.read_backwards_until(|c, _| !c.is_ascii_whitespace(), true)
                },
                TraversalPattern::LowerEnd => {
                    self.read_forwards_until(|c, _| {
                        !c.is_ascii_lowercase() && !c.is_ascii_uppercase()
                    }, false)
                },
                TraversalPattern::UpperEnd => {
                    self.read_forwards_until(|c, _| {
                        c.is_ascii_whitespace()
                    }, false)
                },
                TraversalPattern::To(character) => {
                    self.read_forwards_until(|c, _| c == character, false)
                },
                TraversalPattern::UpperTo(character) => {
                    self.read_backwards_until(|c, _| c == character, false)
                },
                TraversalPattern::Find(character) => {
                    self.read_forwards_until(|c, _| c == character, true)
                },
                TraversalPattern::UpperFind(character) => {
                    self.read_backwards_until(|c, _| c == character, true)
                },
            };

            if hit_newline {
                break;
            }

            match result {
                Ok(Some((_, result, _))) => {
                    final_offset = self.get_offset();
                    if final_offset > initial_offset {
                        combined_result = format!("{}{}", combined_result, result);
                    } else {
                        combined_result = format!("{}{}", result, combined_result);
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

        if initial_offset == final_offset {
            Ok(None)
        } else {
            Ok(Some((initial_offset..final_offset, combined_result)))
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
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true) else {
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
            let Ok(Some((_, result, _))) = self.read_forwards_until(|c, _| c == NEWLINE_CHAR, true) else {
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
    Word,
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
    verb: Option<Verb>,
    noun: Option<Noun>,
    should_clear_command_count: bool,
}

#[derive(PartialEq)]
#[derive(Debug)]
pub struct ViewDumpedData {
    mode: Mode,
    command_count: usize,
    verb: Option<Verb>,
    noun: Option<Noun>,
}

impl View {
    pub fn new(buffer: Box<Buffer>) -> View {
        View {
            buffer: buffer,
            mode: Mode::Normal,
            position: (0, 0),
            state: ViewState::Initial,
            command_count: String::from(""),
            verb: None,
            noun: None,
            should_clear_command_count: false,
        }
    }
    fn clear_command(&mut self) {
        self.command_count = String::from("");
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
        println!("command_count={:?}", self.command_count);
        println!("verb={:?}", self.verb);
        println!("noun={:?}", self.noun);
        println!("should_clear_command_count={:?}", self.should_clear_command_count);
        println!("-------------");

        ViewDumpedData {
            mode: self.mode.clone(),
            command_count: self.command_count.parse::<usize>().unwrap_or(1),
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
        (
            self.state == ViewState::HasVerb || 
            self.state == ViewState::IsInside ||
            self.state == ViewState::IsAround
        )
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
            println!("CHAR: {}", character);
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
                'w' if self.next_can_be_noun() => self.set_noun(Noun::Word),
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

        let range_start = self.position;
        let mut range_end = self.position;
        match self.noun {
            Some(Noun::Character) => {
                // self.buffer.read_to_pattern();
            },
            // Character,
            // Word,
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
            _ => {},
        }

        self.dump();
        self.clear_command();
    }
}


#[cfg(test)]
mod test_engine {
    use super::*;

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
            assert_eq!(buffer.read(3), Ok(String::from("111")));

            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(1);
            assert_eq!(buffer.read(3), Ok(String::from("112")));

            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(2);
            assert_eq!(buffer.read(2), Ok(String::from("12")));

            // Make sure that if at the end, as much data is returned as possible
            let mut buffer = {
                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                Buffer::new_from_tokenscollection(Box::new(result.4))
            };
            buffer.seek(3);
            assert_eq!(buffer.read(5), Ok(String::from("2")));
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
            assert_eq!(buffer.read(3), Ok(String::from("111")));
            buffer.seek(1);
            assert_eq!(buffer.read(3), Ok(String::from("112")));
            buffer.seek(2);
            assert_eq!(buffer.read(2), Ok(String::from("12")));

            // Make sure that if at the end, as much data is returned as possible
            buffer.seek(3);
            assert_eq!(buffer.read(5), Ok(String::from("2")));
        }

        #[test]
        fn it_is_able_to_read_from_plain_text_buffer_repeatedly() {
            // Get a few subranges to make sure they generate the right data
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            buffer.seek(0);
            assert_eq!(buffer.read(3), Ok(String::from("foo")));
            buffer.seek(1);
            assert_eq!(buffer.read(3), Ok(String::from("oo ")));
            buffer.seek(2);
            assert_eq!(buffer.read(5), Ok(String::from("o bar")));
            buffer.seek(6);
            assert_eq!(buffer.read(4), Ok(String::from("r ba")));

            // Make sure that if at the end, as much data is returned as possible
            buffer.seek(9);
            assert_eq!(buffer.read(10), Ok(String::from("az")));
        }

        fn get_first_two_in_tuple(v: Result<Option<(
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

        mod read_forwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == 'b', true)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 4);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == 'b', false)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(buffer.get_offset(), 4);
            }

            #[test]
            fn it_should_seek_by_index_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|_, i| i >= 5, true)),
                    Ok(Some((0..6, "foo ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);
            }

            #[test]
            fn it_should_seek_by_index_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|_, i| i >= 5, false)),
                    Ok(Some((0..5, "foo b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);
            }

            #[test]
            fn it_should_never_match_a_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == 'X', false)),
                    Ok(None)
                );
                assert_eq!(buffer.get_offset(), 0);
            }

            #[test]
            fn it_should_seek_forward_in_sequence() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");

                // First seek to the first space
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == ' ', true)),
                    Ok(Some((0..4, "foo ".to_string())))
                );
                assert_eq!(buffer.get_offset(), 3);

                // Then seek to right before the `a`
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == 'a', false)),
                    Ok(Some((3..5, " b".to_string())))
                );
                assert_eq!(buffer.get_offset(), 5);

                // Then seek by index most of the way to the end
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_forwards_until(|_, i| i >= 9, true)),
                    Ok(Some((5..10, "ar ba".to_string())))
                );
                assert_eq!(buffer.get_offset(), 9);
            }
        }

        mod read_backwards_until {
            use super::*;

            #[test]
            fn it_should_seek_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                buffer.seek(10);
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_backwards_until(|c, _| c == 'r', true)),
                    Ok(Some((11..6, "r baz".to_string())))
                );
                assert_eq!(buffer.get_offset(), 6);
            }

            #[test]
            fn it_should_seek_not_including_matched_char() {
                let mut buffer = Buffer::new_from_literal("foo bar baz");
                buffer.seek(10);
                assert_eq!(
                    get_first_two_in_tuple(buffer.read_backwards_until(|c, _| c == 'r', false)),
                    Ok(Some((11..7, " baz".to_string())))
                );
                assert_eq!(buffer.get_offset(), 6);
            }
        }

        #[test]
        fn it_should_seek_forward_and_backwards_in_sequence() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");

            // First seek to the first space
            assert_eq!(
                get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == ' ', true)),
                Ok(Some((0..4, "foo ".to_string())))
            );
            assert_eq!(buffer.get_offset(), 3);

            // Then seek back a few characters
            assert_eq!(
                get_first_two_in_tuple(buffer.read_backwards_until(|c, _| c == 'f', true)),
                Ok(Some((4..0, "foo ".to_string())))
            );
            assert_eq!(buffer.get_offset(), 0);

            // Then seek to the first space, NOT INCLUDING IT
            assert_eq!(
                get_first_two_in_tuple(buffer.read_forwards_until(|c, _| c == ' ', false)),
                Ok(Some((0..3, "foo".to_string())))
            );
            assert_eq!(buffer.get_offset(), 3);

            // Then seek back a few characters again, NOT INCLUDING IT
            assert_eq!(
                get_first_two_in_tuple(buffer.read_backwards_until(|c, _| c == 'f', false)),
                Ok(Some((4..1, "oo ".to_string())))
            );
            assert_eq!(buffer.get_offset(), 0);
        }
    }

    mod test_view {
        use super::*;

        #[test]
        fn it_should_change_word() {
            let mut buffer = Buffer::new_from_literal("foo.foo bar baz");
            let mut view = buffer.create_view();

            for (input_text, dumped_data) in vec![
                ("cw", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 1,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Word),
                }),
                ("2cw", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Word),
                }),
                ("c2w", ViewDumpedData {
                    mode: Mode::Normal,
                    command_count: 2,
                    verb: Some(Verb::Change),
                    noun: Some(Noun::Word),
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
            ] {
                view.raw_parse_input(input_text, |_| {});
                assert_eq!(view.dump(), dumped_data, "Assertion failed: `{}`", input_text);
                view.reset();
            }
        }
    }
}
