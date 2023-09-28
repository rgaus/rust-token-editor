use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;

use crate::token::*;
use crate::token_match_template::*;

pub struct Buffer {
    document: Box<TokensCollection>,
    offset_stack: Vec<usize>,
}

pub enum TraversalPattern {
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

impl Buffer {
    pub fn new_from_tokenscollection(document: Box<TokensCollection>) -> Buffer {
        Buffer {
            document: document,
            offset_stack: vec![0],
        }
    }
    pub fn new_from_literal(literal: &str) -> Buffer {
        Buffer {
            document: Box::new(TokensCollection::new_unparsed_literal(literal)),
            offset_stack: vec![0],
        }
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
    ) -> Result<Option<String>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token.id;
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
            self.seek(offset + (result_length-1));
            Ok(Some(result[0..result_length-1].to_string()))
        } else {
            self.seek(offset + (result_length-1));
            println!("SEEK: {}", self.get_offset());
            Ok(Some(result))
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
    ) -> Result<Option<String>, String> where F: FnMut(char, usize) -> bool {
        let Some(offset) = self.offset_stack.last() else {
            panic!("offset_stack vector is empty!")
        };
        let Some((token, token_offset)) = self.document.get_by_offset(*offset) else {
            return Err(format!("Cannot get token at offset {} in document!", offset));
        };
        // println!("TOK: {} -> {:?} {}", offset, token, token_offset);

        let mut is_first = true;
        let mut is_done = false;
        let mut result = String::from("");
        let mut pointer_id = token.id;
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

        if !include_matched_char && result.len() > 0 {
            self.seek(offset - (result.len()-1)); // FIXME: I think this is wrong
            Ok(Some(result[1..].to_string()))
        } else {
            self.seek(offset - (result.len()-1));
            Ok(Some(result))
        }
    }

    pub fn read_to_pattern(
        &mut self,
        pattern: TraversalPattern,
        repeat_count: usize,
    ) -> Result<Option<(std::ops::Range<usize>, String)>, String> {
        let initial_offset = self.get_offset();
        let mut final_offset = initial_offset;

        let mut combined_result = String::from("");

        for _index in 0..repeat_count {
            let mut hit_newline = false;
            let result = match pattern {
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
                Ok(Some(result)) => {
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
}


#[cfg(test)]
mod test_engine {
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

    mod read_forwards_until {
        use super::*;

        #[test]
        fn it_should_seek_including_matched_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            assert_eq!(
                buffer.read_forwards_until(|c, _| c == 'b', true),
                Ok(Some("foo b".to_string()))
            );
            assert_eq!(buffer.get_offset(), 4);
        }

        #[test]
        fn it_should_seek_not_including_matched_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            assert_eq!(
                buffer.read_forwards_until(|c, _| c == 'b', false),
                Ok(Some("foo ".to_string()))
            );
            assert_eq!(buffer.get_offset(), 4);
        }

        #[test]
        fn it_should_seek_by_index_including_matched_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            assert_eq!(
                buffer.read_forwards_until(|_, i| i >= 5, true),
                Ok(Some("foo ba".to_string()))
            );
            assert_eq!(buffer.get_offset(), 5);
        }

        #[test]
        fn it_should_seek_by_index_not_including_matched_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            buffer.seek(0);
            assert_eq!(
                buffer.read_forwards_until(|_, i| i >= 5, false),
                Ok(Some("foo b".to_string()))
            );
            assert_eq!(buffer.get_offset(), 5);
        }

        #[test]
        fn it_should_never_match_a_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            assert_eq!(
                buffer.read_forwards_until(|c, _| c == 'X', false),
                Ok(None)
            );
            assert_eq!(buffer.get_offset(), 0);
        }

        #[test]
        fn it_should_seek_forward_in_sequence() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");

            // First seek to the first space
            assert_eq!(
                buffer.read_forwards_until(|c, _| c == ' ', true),
                Ok(Some("foo ".to_string()))
            );
            assert_eq!(buffer.get_offset(), 3);

            // Then seek to right before the `a`
            assert_eq!(
                buffer.read_forwards_until(|c, _| c == 'a', false),
                Ok(Some(" b".to_string()))
            );
            assert_eq!(buffer.get_offset(), 5);

            // Then seek by index most of the way to the end
            assert_eq!(
                buffer.read_forwards_until(|_, i| i >= 9, true),
                Ok(Some("ar ba".to_string()))
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
                buffer.read_backwards_until(|c, _| c == 'r', true),
                Ok(Some("r baz".to_string()))
            );
            assert_eq!(buffer.get_offset(), 6);
        }

        #[test]
        fn it_should_seek_not_including_matched_char() {
            let mut buffer = Buffer::new_from_literal("foo bar baz");
            buffer.seek(10);
            assert_eq!(
                buffer.read_backwards_until(|c, _| c == 'r', false),
                Ok(Some(" baz".to_string()))
            );
            assert_eq!(buffer.get_offset(), 6);
        }
    }

    #[test]
    fn it_should_seek_forward_and_backwards_in_sequence() {
        let mut buffer = Buffer::new_from_literal("foo bar baz");

        // First seek to the first space
        assert_eq!(
            buffer.read_forwards_until(|c, _| c == ' ', true),
            Ok(Some("foo ".to_string()))
        );
        assert_eq!(buffer.get_offset(), 3);

        // Then seek back 2 characters
        assert_eq!(
            buffer.read_backwards_until(|c, _| c == 'f', true),
            Ok(Some("foo ".to_string()))
        );
        assert_eq!(buffer.get_offset(), 0);
    }
}
