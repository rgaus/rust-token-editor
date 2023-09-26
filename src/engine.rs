use crate::token::*;
use crate::token_match_template::*;

pub struct Buffer {
    document: Box<TokensCollection>,
    offset_stack: Vec<usize>,
}

impl Buffer {
    pub fn new_from_tokenscollection(document: Box<TokensCollection>) -> Buffer {
        Buffer {
            document: document,
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
    pub fn read_forwards_until(
        &mut self,
        needle_func: fn(char) -> bool,
        include_matched_char: bool,
    ) -> Result<String, String> {
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
                    result = format!("{}{}", result, character);
                    if needle_func(character) {
                        is_done = true;
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


        if !include_matched_char && result.len() > 0 {
            self.seek(offset + result.len()-1);
            Ok(result[0..result.len()-1].to_string())
        } else {
            self.seek(offset + result.len());
            Ok(result)
        }
    }

    // Reads the buffer character by character in reverse from the current location to the start of
    // the document until the specified `needle_func` passes, and then returns the data that has been read.
    //
    // If `include_matched_char` is true, the final matched character is included, otherwise it is
    // not.
    pub fn read_backwards_until(
        &mut self,
        needle_func: fn(char) -> bool,
        include_matched_char: bool,
    ) -> Result<String, String> {
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
                for (index, character) in literal_text.chars().rev().enumerate() {
                    if is_first && index > token_offset {
                        continue;
                    }
                    result = format!("{}{}", character, result);
                    if needle_func(character) {
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


        if !include_matched_char && result.len() > 0 {
            self.seek(offset - result.len()-1);
            Ok(result[0..result.len()-1].to_string())
        } else {
            self.seek(offset - result.len());
            Ok(result)
        }
    }
}
