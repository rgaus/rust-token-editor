use colored::{Color, Colorize};
use rangemap::RangeMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use uuid::Uuid;

use crate::token_match_template::*;


#[derive(Debug)]
pub struct TokensCollection {
    pub tokens: Vec<Box<Token>>,
    token_match_templates_map: Rc<TokenMatchTemplateMap>,

    // A cache that stores the character offset of each token's start and end in the final output
    // string, mapped FORWARDS. Use this to figure out where a token will be in the output.
    offset_cache: RefCell<HashMap<uuid::Uuid, (usize, usize)>>,

    // A cache that stores the character offset of each token's start and end in the final output
    // string, mapped BACKWARDS. Use this to query for a token at a specific character offset.
    tokens_by_start_offset_cache: RefCell<RangeMap<usize, uuid::Uuid>>,

    // Stores the offset of set locations in the document which the system attempts to keep in the
    // same locations as mofidicaations are made.
    bookmarks: HashMap<uuid::Uuid, (uuid::Uuid, usize)>,
}

impl TokensCollection {
    pub fn new(tokens: Vec<Box<Token>>, token_match_templates_map: Rc<TokenMatchTemplateMap>) -> TokensCollection {
        TokensCollection {
            tokens: tokens,
            token_match_templates_map: token_match_templates_map,
            offset_cache: RefCell::new(HashMap::new()),
            tokens_by_start_offset_cache: RefCell::new(RangeMap::new()),
            bookmarks: HashMap::new(),
        }
    }
    pub fn new_empty() -> TokensCollection {
        TokensCollection::new(vec![], Rc::new(HashMap::new()))
    }
    pub fn new_unparsed_literal(literal: &str) -> TokensCollection {
        let mut document = TokensCollection::new_empty();
        document.push(Box::new(Token {
            id: uuid::Uuid::new_v4(),
            template: TokenMatchTemplateMatcher::Skipped,
            literal: Some(String::from(literal)),
            matches: HashMap::new(),
            effects: vec![],
            events: TokenEvents::new_empty(),
            next_id: None,
            previous_id: None,
            parent_id: None,
            children_ids: vec![],
        }));

        document
    }
    pub fn push(&mut self, token: Box<Token>) {
        self.tokens.push(token);
    }

    pub fn get_by_id<'a>(&'a self, id: uuid::Uuid) -> Option<&'a Box<Token>> {
        self.tokens.iter().find(|t| t.id == id)
    }

    pub fn get_by_id_mut<'a, F>(
        &'a mut self,
        id: uuid::Uuid,
        mut closure: F,
    ) where F: FnMut(&mut Token) {
        for mut token in &mut self.tokens {
            if token.id == id {
                closure(&mut token);
            }
        }
    }

    pub fn replace<'a>(
        &'a mut self,
        id: uuid::Uuid,
        new_token: Box<Token>,
    ) -> bool {
        let index_or_none = {
            let mut found = false;
            let mut index = 0;
            for token in &self.tokens {
                println!("TOKEN? {} {}", token.id, id);
                if token.id == id {
                    found = true;
                    break;
                };
                index += 0;
            };

            if found {
                Some(index)
            } else {
                None
            }
        };
        let Some(index) = index_or_none else {
            return false;
        };

        self.tokens[index] = new_token;
        true
    }

    pub fn remove(&mut self, id: uuid::Uuid) -> bool {
        let original_offset = self.compute_offset(id);

        let index_or_none = {
            let mut found = false;
            let mut index = 0;
            for token in &self.tokens {
                if token.id == id {
                    found = true;
                    break;
                };
                index += 1;
            };

            if found {
                Some(index)
            } else {
                None
            }
        };
        let Some(index) = index_or_none else {
            return false;
        };

        // Clear all caches of data at or after this token
        self.reset_caches_for_and_after(original_offset);

        self.tokens.remove(index);
        true
    }

    // Returns the node at the very top / start of the token collection.
    // NOTE: This node is guaranteed to have an offset of 0.
    pub fn get_first_root_node(&self) -> Option<&Box<Token>> {
        for token in &self.tokens {
            if token.previous_id == None {
                return Some(token);
            }
        }
        None
    }

    // Returns the node at the very end of the token collection.
    // In a tree representation, this node is the leaf node furthest to the right.
    pub fn get_final_node(&self) -> Option<&Box<Token>> {
        for token in &self.tokens {
            if token.next_id == None {
                return Some(token);
            }
        }
        None
    }

    // Queries the token collection and returns the Box<Token> that covers the `input_offset` specified,
    // or None. If a token is found, the offset from the start of the token that `input_offset`
    // refers to is also returned.`
    pub fn get_by_offset(&self, input_offset: usize) -> Option<(&Box<Token>, usize)> {
        // Prior to searching for a matching node, make sure the cache has at least one item in it
        // first as a base case. The root node is always at zero so it will always be before any
        // other node.
        let is_empty = self.tokens_by_start_offset_cache.borrow().is_empty();
        if is_empty {
            let Some(first_root_node) = self.get_first_root_node() else {
                return None;
            };
            let first_root_node_length = match &first_root_node.literal {
                Some(literal) => literal.len(),
                None => 0,
            };
            let first_root_node_id = first_root_node.id;
            self.tokens_by_start_offset_cache.borrow_mut().insert(0..first_root_node_length+1, first_root_node_id);
            self.offset_cache.borrow_mut().insert(first_root_node_id, (0, first_root_node_length));
        }

        // println!("GET BY OFFSET: {}", input_offset);
        if let Some((offset_range, token_id)) = self.tokens_by_start_offset_cache.borrow().get_key_value(&input_offset) {
            let offset_into_token = input_offset - offset_range.start;
            let Some(token) = self.get_by_id(*token_id) else {
                return None;
            };
            return Some((token, offset_into_token));
        }

        // If a pre-cached value wasn't found, then figure out the next earliest token that is
        // cached, and start computing from there
        let last_gap_start = match self.tokens_by_start_offset_cache.borrow().gaps(&(0..input_offset+1)).last() {
            Some(last_gap) => last_gap.start,
            None => 0,
        };
        // println!("CACHE: {:?} {:?} {}", self.tokens_by_start_offset_cache, self.tokens_by_start_offset_cache.borrow().gaps(&(0..input_offset+1)).last(), last_gap_start);

        let previous_cached_token_start = if last_gap_start > 0 { last_gap_start - 1 } else { 0 };
        let previous_cached_token_id = {
            let tokens_by_start_offset_cache = self.tokens_by_start_offset_cache.borrow();
            let Some(previous_cached_token_id) = tokens_by_start_offset_cache.get(&previous_cached_token_start) else {
                return None;
            };
            previous_cached_token_id.clone()
        };
        let Some(previous_cached_token) = self.get_by_id(previous_cached_token_id) else {
            return None;
        };

        let mut pointer_id = previous_cached_token.next_id;
        let mut offset = previous_cached_token_start;
        // println!("STARTING AT: {} => {} {:?}", offset, input_offset, previous_cached_token);
        while input_offset > 0 {
            let Some(pointer_id_unwrapped) = pointer_id else {
                return None;
            };
            let (range_start, range_end, pointer_length, pointer_next_id) = {
                let Some(pointer) = self.get_by_id(pointer_id_unwrapped) else {
                    return None;
                };
                // println!("POINTER: {:?}", pointer.literal);
                let pointer_length = match &pointer.literal {
                    Some(literal) => literal.len(),
                    None => 0,
                };
                let end_offset = if pointer_length > 0 {
                    offset + pointer_length + 1
                } else {
                    offset
                };
                (offset, end_offset, pointer_length, pointer.next_id)
            };
            // println!("STATUS: {} {} {}", range_start, range_end, pointer_length);

            // Exclude adding zero length tokens to the cache, since those are not indexable by
            // offset since it's impossible to be "inside" them
            let range = range_start..range_end;
            if !range.is_empty() {
                // println!("INSERT: {:?} {:?}", range, pointer_id_unwrapped);
                self.tokens_by_start_offset_cache.borrow_mut().insert(range, pointer_id_unwrapped);
                self.offset_cache.borrow_mut().insert(pointer_id_unwrapped, (range_start, range_end));
            };

            // Once the offset gets to the offset that the user was looking for, we're done
            // if offset != 0 && (offset + pointer_length) >= input_offset {
            if (offset + pointer_length) >= input_offset {
                // println!("FOUND: {} + {} >= {}", offset, pointer_length, input_offset);
                break;
            };

            pointer_id = pointer_next_id;
            offset += pointer_length;
        };

        // The token has been found!
        let Some(pointer_id_unwrapped) = pointer_id else {
            return None;
        };
        println!("FOO: {} - {}", input_offset, offset);
        let offset_into_token = input_offset - offset;
        let Some(token) = self.get_by_id(pointer_id_unwrapped) else {
            return None;
        };
        return Some((token, offset_into_token));
    }

    // Given a token id, returns the offset within the final output text for the start of the token
    pub fn compute_offset(&self, id: uuid::Uuid) -> usize {
        if let Some((cached_offset_start, _)) = self.offset_cache.borrow().get(&id) {
            return *cached_offset_start;
        }

        let (token_length, previous_id, previous_length) = {
            let Some(token) = self.get_by_id(id) else {
                return 0;
            };
            let token_length = match &token.literal {
                Some(literal) => literal.len(),
                None => 0,
            };
            let Some(previous_id) = token.previous_id else {
                return 0;
            };
            let Some(previous) = self.get_by_id(previous_id) else {
                return 0;
            };

            let previous_length = match &previous.literal {
                Some(literal) => literal.len(),
                None => 0,
            };

            (token_length, previous_id, previous_length)
        };

        // The current offset is equal to the previous offset plus the length of the previous token
        let previous_offset = self.compute_offset(previous_id);
        let offset = previous_offset + previous_length;

        self.offset_cache.borrow_mut().insert(id, (offset, offset + token_length));
        if token_length > 0 {
            self.tokens_by_start_offset_cache.borrow_mut().insert(offset..offset + token_length, id);
        }
        offset
    }

    pub fn reset_caches_for_and_after(&mut self, original_offset: usize) -> bool {
        // Walk through the token collection, removing all cached elements at and after `token_id`
        // from `self.offset_cache`
        // self.offset_cache.borrow_mut().clear();

        self.offset_cache.borrow_mut().retain(|_id, (start_offset, end_offset)| {
            *start_offset >= original_offset || *end_offset >= original_offset
        });

        // let mut pointer_id = token_id;
        // loop {
        //     let Some(pointer) = self.get_by_id(pointer_id) else {
        //         break;
        //     };

        //     self.offset_cache.borrow_mut().remove(&pointer_id);

        //     let mut should_break = true;
        //     if let Some(next_pointer_id) = pointer.next_id {
        //         pointer_id = next_pointer_id;
        //         should_break = false;
        //     };

        //     if should_break {
        //         break;
        //     };
        // };

        // Delete the whole range of data starting at `offset` and going all the way to the end of
        // `tokens_by_start_offset_cache`.
        // self.tokens_by_start_offset_cache.borrow_mut().clear();
        {
            // let offset = self.compute_offset(token_id);
            println!("CLEARING tokens_by_start_offset_cache: {:?}", self.tokens_by_start_offset_cache);
            let maximum_cached_offset = {
                let result_range = self.tokens_by_start_offset_cache
                    .borrow()
                    .iter()
                    .map(|(range, _)| range)
                    .fold(0..original_offset, |rangea, rangeb| {
                        println!("{rangea:?} -> {rangeb:?}");
                        if rangea.end > rangeb.end {
                            rangea
                        } else {
                            rangeb.clone()
                        }
                    });
                result_range.end
            };

            println!("\t range={original_offset}..{maximum_cached_offset}");

            if maximum_cached_offset > original_offset {
                self.tokens_by_start_offset_cache.borrow_mut().remove(original_offset..maximum_cached_offset);
            }
        };

        println!("RESET_CACHES_FOR_AND_AFTER({original_offset}) => {:?}", self.offset_cache.borrow());

        true
    }

    // When called, modifies the token with the given id to contain the new `literal` value
    // specified as a parameter.
    //
    // This causes the system to reparse the token, doing an in-place replace within the token
    // collection. This could result in a few possible outcomes:
    // 1. The token reparses as exactly the same format as it did before. Think changing `11` => `12`
    // 2. The token reparses completely differently. Think changing `11` => `11+22`
    //
    // This function returns the id of the first child token of the reparsed token tree. In case
    // #1, this token should be equivilent to the old token in all ways but the literal value (and
    // maybe id). In case #2, expect to find a whole new token subtree following this node!
    pub fn change_token_literal_text(
        &mut self,
        token_id: uuid::Uuid,
        new_text: String,
    ) -> Result<Option<uuid::Uuid>, String> {
        let original_offset = self.compute_offset(token_id);

        println!("\n\nCHANGE_TOKEN_LITERAL_TEXT({token_id:?}, '{new_text}') : ----\n{}\n-------", self.stringify());
        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.get_by_id(pointer_id) else {
                return Err(format!("Cannot find token with id {}", token_id));
            };

            if pointer.literal.is_some() {
                break;
            }

            if let Some(next_id) = pointer.next_id {
                pointer_id = next_id;
            } else {
                return Err(format!("Attempted to change token {token_id}, but this token has a literal of None and no token afterwards has text contents!"));
            }
        }

        self.get_by_id_mut(pointer_id, |token| {
            if pointer_id == token_id {
                // If the token selected was the token with literal text contents, then swap the
                // token literal for the new data
                token.literal = Some(new_text.clone());
            } else {
                // If a token with text contents was selected AFTER the token specified, then
                // prepend the literal text in front of the new token
                token.literal = Some(format!(
                    "{new_text}{}",
                    if let Some(literal) = &token.literal { literal.clone() } else { String::from("") },
                ));
            }
        });

        let old_token = self.get_by_id(pointer_id).unwrap();
        // println!("FOUND NEXT TOKEN WITH CONTENTS: {old_token:?}");

        // Create a clone of the token to modify in-memory
        let mut working_token = old_token.clone();

        // Clear all caches of data at or after this token
        self.reset_caches_for_and_after(original_offset);

        // Reparse the token, to try to convert it into something now that it has been modified
        working_token.reparse(self)
    }

    // When called, removes a token from the token tree. When removed, all token links are updated
    // so that this removed token is now omitted.
    //
    // If one attempts to remove a non-leaf token, this function returns an `Err`
    pub fn remove_leaf(&mut self, token_id: uuid::Uuid) -> Result<bool, String> {
        let original_offset = self.compute_offset(token_id);

        let old_token_data = {
            let Some(old_token) = self.get_by_id(token_id) else {
                return Err(format!("Cannot find token {}", token_id));
            };

            let last_deep_child = old_token.deep_last_child(self);

            Ok((
                old_token.next_id,
                old_token.previous_id,
                old_token.parent_id,
                last_deep_child,
            ))
        };

        match old_token_data {
            Ok((
                old_token_next_id,
                old_token_previous_id,
                old_token_parent_id,
                last_deep_child,
            )) => {
                // Update the pointer on the child AFTER old_token that points to the final deep child
                if let Some(last_deep_child) = last_deep_child {
                    if let Some(token_after_old_token_id) = last_deep_child.next_id {
                        self.get_by_id_mut(token_after_old_token_id, |token_after_old_token| {
                            token_after_old_token.previous_id = old_token_previous_id;
                        })
                    }
                }

                // Update all pointers that point to `old_token`
                if let Some(previous_id) = old_token_previous_id {
                    self.get_by_id_mut(previous_id, |previous| {
                        previous.next_id = old_token_next_id;
                    });
                }
                if let Some(next_id) = old_token_next_id {
                    self.get_by_id_mut(next_id, |next| {
                        next.previous_id = old_token_previous_id;
                    });
                }
                if let Some(parent_id) = old_token_parent_id {
                    self.get_by_id_mut(parent_id, |parent| {
                        parent.children_ids.retain(|child_id| *child_id != token_id);
                    });
                }

                // Remove the token once it's no longer depended on by anything else
                self.remove(token_id);

                // Clear all caches of data at or after this token
                self.reset_caches_for_and_after(original_offset);

                Ok(true)
            },
            Err(err) => Err(err),
        }
    }

    pub fn add_bookmark(&mut self, offset: usize) -> Result<uuid::Uuid, String> {
        let id = Uuid::new_v4();

        let Some((token, token_offset)) = self.get_by_offset(offset) else {
            return Err(format!("Error in Document.add_bookmark: cannot find token at offset {offset}!"));
        };
        let token_id = token.id;

        self.bookmarks.insert(id, (token_id, token_offset));
        Ok(id)
    }

    pub fn get_bookmark_offset(&mut self, bookmark_id: uuid::Uuid) -> Result<usize, String> {
        let Some((token_id, token_offset)) = self.bookmarks.get(&bookmark_id) else {
            return Err(format!("Error in Document.get_bookmark_offset: cannot find bookmark with id {bookmark_id}!"));
        };
        let token_id_cloned = token_id.clone();
        let token_offset_cloned = token_offset.clone();
        let offset = self.compute_offset(token_id_cloned);
        Ok(offset + token_offset_cloned)
    }

    pub fn remove_bookmark(&mut self, bookmark_id: uuid::Uuid) -> Result<(), String> {
        self.bookmarks.remove(&bookmark_id);
        Ok(())
    }

    // // Starting at the given token + offset within the literal of that offset, scan forward
    // // character by character within token stream. Once `needle_fn` returns `true`, then 
    // pub fn remove_starting_at_token_until<F>(
    //     &mut self,
    //     token_id: uuid::Uuid,
    //     token_start_offset: usize,
    //     mut needle_fn: F,
    // ) where F: FnMut(char #<{(| character |)}>#, usize #<{(| offset |)}>#) -> bool {
    //     let mut result = String::from("");
    //     let mut pointer_id = starting_token_id;
    //     loop {
    //         let Some(mut pointer) = self.get_by_id(pointer_id) else {
    //             break;
    //         };
    //         if let Some(literal_text) = &pointer.literal {
    //             result = format!("{}{}", result, literal_text);
    //         };
    //         if let Some(next_pointer_id) = pointer.next_id {
    //             pointer_id = next_pointer_id;
    //         } else {
    //             break;
    //         }
    //     }
    //
    //     result
    // }

    // When called with a token node id, walks along through all `next_id` links,
    // concatenating all literal values in each token to generate the contents of
    // the document.
    pub fn stringify_to_end(&self, starting_token_id: uuid::Uuid) -> String {
        let mut result = String::from("");
        let mut pointer_id = starting_token_id;
        loop {
            let Some(pointer) = self.get_by_id(pointer_id) else {
                break;
            };
            if let Some(literal_text) = &pointer.literal {
                result = format!("{}{}", result, literal_text);
            };
            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
            } else {
                break;
            }
        }

        result
    }

    // When called with a token node id, walks along through all `next_id` links,
    // concatenating all literal values in each token to generate the contents of
    // the document until AT LEAST `at_least_offset` characters have been
    // generated, or the end of the document is reached.
    //
    // An example use case of this function may be to get the currently visible region of a
    // document to show to a user.
    pub fn stringify_for_offset(&self, starting_token_id: uuid::Uuid, at_least_offset: usize) -> String {
        let mut result = String::from("");
        let mut pointer_id = starting_token_id;
        loop {
            if result.len() > at_least_offset {
                return result;
            };
            let Some(pointer) = self.get_by_id(pointer_id) else {
                break;
            };
            if let Some(literal_text) = &pointer.literal {
                result = format!("{}{}", result, literal_text);
            };
            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
            } else {
                break;
            }
        }

        result
    }

    pub fn stringify_for_selection(
        &self,
        starting_token_id: uuid::Uuid,
        starting_token_offset: usize,
        char_count: usize,
    ) -> String {
        if char_count == 0 {
            return String::from("");
        }

        let mut index = 0;
        let mut result = String::from("");
        let mut pointer_id = starting_token_id;
        loop {
            let Some(pointer) = self.get_by_id(pointer_id) else {
                break;
            };
            if let Some(literal_text) = &pointer.literal {
                for character in literal_text.chars() {
                    if index < starting_token_offset {
                        index += 1;
                        continue;
                    }
                    // println!("stringify_for_selection: {index} {character} starting_token_offset={starting_token_offset} char_count={char_count}");
                    result = format!("{}{}", result, character);
                    if index >= starting_token_offset + char_count - 1 {
                        return result;
                    };
                    index += 1;
                }
            };
            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
            } else {
                break;
            }
        }

        result
    }

    // Outputs the entire document as a text string.
    //
    // NOTE: This function should almost never be used outside of a testing context, as it requires
    // allocating potentially a very large string in the heap!
    pub fn stringify(&self) -> String {
        let Some(first_root_node) = self.get_first_root_node() else {
            return String::from("");
        };

        self.stringify_to_end(first_root_node.id)
    }

    pub fn debug_stringify_highlight(&self, offset_start: usize, offset_end: usize) -> String {
        let Some(first_root_node) = self.get_first_root_node() else {
            return String::from("");
        };
        let starting_token_id = first_root_node.id;

        let mut count = 0;
        let mut index = 0;
        let mut result = String::from("");
        let mut pointer_id = starting_token_id;
        loop {
            let color = if count % 2 == 0 {
                Color::TrueColor { r: 70, g: 70, b: 0 }
            } else { Color::TrueColor { r: 0, g: 70, b: 70 } };
            let Some(pointer) = self.get_by_id(pointer_id) else {
                break;
            };
            // println!("{:?}\t{:?} -> {:?} -> {:?}", pointer.literal, pointer.previous_id, pointer.id, pointer.next_id);
            // println!("{}", count);
            if let Some(literal_text) = &pointer.literal {
                count += 1;
                for character in literal_text.chars() {
                    let character_as_string = if character == '\n' {
                        String::from(" ")
                    } else {
                        format!("{}", character)
                    };
                    let colored_character = if index >= offset_start && index < offset_end {
                        character_as_string.red().on_white()
                    } else {
                        // character_as_string.normal()
                        character_as_string.on_color(color)
                    };
                    result = format!("{}{}", result, colored_character);
                    if character == '\n' {
                        result = format!("{}\n", result);
                    }
                    index += 1;
                }
                // FIXME: make sure append at the end of the document can be rendered
            };
            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
            } else {
                break;
            }
        }

        result
    }

    pub fn debug_token_tree_string(&self) -> String {
        let mut lines: Vec<(String, &Token)> = vec![];

        // Generate each line in the diagram
        let mut expected_next_token_id: Option<uuid::Uuid> = None;
        let mut expected_previous_token_id: Option<uuid::Uuid> = None;
        for token in &self.tokens {
            if token.parent_id.is_none() {
                // parent_id_count += 1;
                lines.extend(self.debug_token_tree_string_recurse_into_level(
                    &token,
                    0,
                    String::from(""),
                    &mut expected_next_token_id,
                    &mut expected_previous_token_id,
                ));
            }
        }

        // Generate arrows that should be rendered
        let mut arrows: Vec<(usize, usize)> = vec![];
        for (index, (line, token)) in lines.iter().enumerate() {
            let Some(next) = token.next(&self) else {
                continue;
            };
            let Some(next_index) = lines.iter().position(|(_, search_token)| search_token.id == next.id) else {
                continue;
            };

            if index+1 != next_index {
                arrows.push((index, next_index));
            }
        }

        // Draw the arrows
        let mut prefixes: Vec<String> = lines.iter().map(|_| String::from("")).collect();
        let min_arrow_space_chars = String::from("  ");
        for (start_index, end_index) in arrows {
            let smaller_index = std::cmp::min(start_index, end_index);
            let larger_index = std::cmp::max(start_index, end_index);

            // Within the prefixes start_index..end_index, are two spaces free at the same
            // index in each line?
            let first_free_index_result = prefixes[smaller_index].chars().enumerate().find(|(index, _)| {
                for p in smaller_index..larger_index {
                    let prefix = prefixes[p].clone();
                    println!("FOO: {}", index);

                    let start_index = *index;
                    let mut end_index = index+(min_arrow_space_chars.len()-1);
                    if end_index > prefix.len()-1 {
                        end_index = prefix.len();
                    }

                    let range_contains_non_whitespace_chars = prefix[start_index..end_index].chars().find(
                        |c| *c != ' '
                    ).is_none();
                    if range_contains_non_whitespace_chars {
                        return false;
                    }
                }

                true
            });

            // Add space to front if needed so that a new arrow can be drawn in "empty space"
            let first_free_index = if let Some((index, _)) = first_free_index_result {
                index
            } else {
                prefixes = prefixes.iter().map(|l| format!("{min_arrow_space_chars}{l}")).collect();
                0
            };

            // Draw a "*----" on the start_index
            for char_index in first_free_index..prefixes[start_index].len() {
                let char_at_index = prefixes[start_index].chars().nth(char_index).unwrap();
                if char_at_index == '>' {
                    continue;
                }
                prefixes[start_index].replace_range(
                    char_index..char_index+1,
                    if char_index == first_free_index {
                        if start_index == smaller_index { "," } else { "`" }
                    } else { "-" }
                );
            }

            // Draw a "|" on all index between the start_index and end_index
            for index in smaller_index+1..larger_index {
                prefixes[index].replace_range(first_free_index..first_free_index+1, "|");
            }

            // Draw a "*--->" on the end_index
            for char_index in first_free_index..prefixes[end_index].len() {
                let char_at_index = prefixes[start_index].chars().nth(char_index).unwrap();
                if char_at_index == '>' {
                    continue;
                }
                let new_char = if char_index == prefixes[end_index].len()-1 {
                    ">"
                } else if char_index == first_free_index {
                    if end_index == smaller_index { "," } else { "`" }
                } else {
                    "-"
                };
                prefixes[end_index].replace_range(char_index..char_index+1, new_char);
            }
        }

        // Put everything together

        // NOTE: Add whitespace before the header row equal to the prefix length to align the
        // headers properly
        let mut whitespace_before_header_row = (
            0..prefixes[0].len()
        ).map(|_| String::from(" ")).collect::<Vec<String>>().join("");
        if whitespace_before_header_row.len() > 0 {
            // Add a space between the arrows and each line if arrows are being drawn
            whitespace_before_header_row = format!("{whitespace_before_header_row} ");
            prefixes = prefixes.iter().map(|l| format!("{l} ")).collect();
        }

        let acc = format!(
            "Token Tree:\n{whitespace_before_header_row}N P Details\n{}",
            lines
                .iter()
                .enumerate()
                .map(|(index, (line, _))| format!("{}{}",
                    prefixes[index],
                    // prefixes[index].replace("-", "═").replace("|", "║").replace("`", "╚").replace(",", "╔"),
                    line
                )).collect::<Vec<String>>().join("\n"),
        );

        // acc = format!("{acc}\nNumber of top level nodes: {}", parent_id_count);
        acc
    }
    fn debug_token_tree_string_recurse_into_level<'a>(
        &'a self,
        token: &'a Box<Token>,
        depth: usize,
        prefix: String,
        expected_next_token_id: &mut Option<uuid::Uuid>,
        expected_previous_token_id: &mut Option<uuid::Uuid>,
    ) -> Vec<(String, &Token)> {
        let mut lines: Vec<(String, &Token)> = vec![];
        let colored_star = if depth % 2 == 0 { "*".red() } else { "*".blue() };
        let colored_pipe = if depth % 2 == 0 { "|".red() } else { "|".blue() };

        let colored_next_indicator = if *expected_next_token_id == Some(token.id) { "↓".white() } else { "•".red() };
        let colored_previous_indicator = if *expected_previous_token_id == token.previous_id { "↑".white() } else { "•".red() };

        let row = format!(
            "{colored_next_indicator} {colored_previous_indicator} {prefix}{colored_star} {}\t(children={}, next={}, prev={})\t {} {:?}",
            token.abbreviated_id(),
            token.children_ids.len(),
            token.next(&self).map(|n| n.abbreviated_id().cyan()).unwrap_or(
                if let Some(next_id) = token.next_id {
                    format!( "<missing: {}>", &format!("{next_id}")[..3]).red()
                } else { "<empty>".red() }
            ),
            token.previous(&self).map(|p| p.abbreviated_id().yellow()).unwrap_or(
                if let Some(prev_id) = token.previous_id {
                    format!( "<missing: {}>", &format!("{prev_id}")[..3]).red()
                } else { "<empty>".red() }
            ),
            &(String::from(format!("{:?}", token.template))[..7]),
            token.effects,
        );
        *expected_next_token_id = token.next_id;
        *expected_previous_token_id = Some(token.id);

        lines.push((
            format!("{row}\t{:?}", token.literal),
            &token,
        ));

        let Some(children) = token.children(&self) else {
            return lines;
        };
        for child in children {
            let next_depth = depth + 1;
            let next_prefix = format!("{prefix}{} ", colored_pipe);
            lines.extend(self.debug_token_tree_string_recurse_into_level(
                child,
                next_depth,
                next_prefix,
                expected_next_token_id,
                expected_previous_token_id,
            ));
        }

        lines
    }
}


#[derive(Debug)]
#[derive(Clone)]
pub enum TokenMatchTemplateMatcher {
    Raw(&'static str, Option<TokenEvents>),
    Reference(&'static str, Option<TokenEvents>),
    Regex(regex::Regex, Option<regex::Regex>, Option<TokenEvents>),
    Any(Vec<TokenMatchTemplateMatcher>, Option<TokenEvents>),
    Sequence(Vec<TokenMatchTemplateMatcher>, Option<TokenEvents>),
    RepeatCount(Box<TokenMatchTemplateMatcher>, usize, usize, Option<TokenEvents>),
    RepeatOnceToForever(Box<TokenMatchTemplateMatcher>, Option<TokenEvents>),
    RepeatZeroToForever(Box<TokenMatchTemplateMatcher>, Option<TokenEvents>),

    Skipped,
}

impl TokenMatchTemplateMatcher {
    pub fn raw(text: &'static str) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Raw(text, None)
    }
    pub fn raw_with_events(text: &'static str, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Raw(text, Some(events))
    }

    pub fn reference(name: &'static str) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Reference(name, None)
    }
    pub fn reference_with_events(name: &'static str, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Reference(name, Some(events))
    }

    pub fn regex(regex: regex::Regex) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Regex(regex, None, None)
    }
    pub fn regex_and_negation(regex: regex::Regex, negated_regex: regex::Regex) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Regex(regex, Some(negated_regex), None)
    }
    pub fn regex_with_events(
        regex: regex::Regex,
        events: TokenEvents,
    ) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Regex(regex, None, Some(events))
    }
    pub fn regex_and_negation_with_events(
        regex: regex::Regex,
        negated_regex: regex::Regex,
        events: TokenEvents,
    ) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Regex(regex, Some(negated_regex), Some(events))
    }

    pub fn any(tokens: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Any(tokens, None)
    }
    pub fn any_with_events(tokens: Vec<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Any(tokens, Some(events))
    }

    pub fn sequence(tokens: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Sequence(tokens, None)
    }
    pub fn sequence_with_events(tokens: Vec<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::Sequence(tokens, Some(events))
    }

    pub fn repeat_count(token: Box<TokenMatchTemplateMatcher>, min_repeats: usize, max_repeats: usize) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatCount(token, min_repeats, max_repeats, None)
    }
    pub fn repeat_count_with_events(
        token: Box<TokenMatchTemplateMatcher>,
        min_repeats: usize,
        max_repeats: usize,
        events: TokenEvents,
    ) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatCount(token, min_repeats, max_repeats, Some(events))
    }

    pub fn repeat_once_to_forever(token: Box<TokenMatchTemplateMatcher>) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatOnceToForever(token, None)
    }
    pub fn repeat_once_to_forever_with_events(token: Box<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatOnceToForever(token, Some(events))
    }

    pub fn repeat_zero_to_forever(token: Box<TokenMatchTemplateMatcher>) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatZeroToForever(token, None)
    }
    pub fn repeat_zero_to_forever_with_events(token: Box<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplateMatcher {
        TokenMatchTemplateMatcher::RepeatZeroToForever(token, Some(events))
    }
}


#[derive(Debug)]
#[derive(Clone)]
pub struct TokenMatch {
    pub start: usize,
    pub end: usize,
    pub global_start: usize,
    pub global_end: usize,
    pub string: String,
}

#[derive(Debug)]
#[derive(Clone)]
pub enum TokenEffect {
    // A DeclareAssignmentContainer indicates an identifier is being assigned
    // It should contain within it one DeclareIdentifier and one DeclareExpression
    DeclareAssignmentContainer,
    DeclareIdentifier(String),
    DeclareExpression(String),
    DeclareMember { name: String, value: String },
    DeclareLexicalScope,
}

#[derive(Debug)]
#[derive(Clone)]
pub struct TokenEvents {
    pub on_enter: Option<fn(token: &mut Box<Token>)>,
    pub on_leave: Option<fn(
        token: &mut Box<Token>,
        tokens_collection: &mut TokensCollection,
    )>,
}

impl TokenEvents {
    pub fn new_empty() -> TokenEvents {
        TokenEvents { on_enter: None, on_leave: None }
    }
    pub fn combine(a: TokenEvents, b: TokenEvents) -> TokenEvents {
        let on_enter = match (a.on_enter, b.on_enter) {
            (None, None) => None,
            (Some(a_on_enter), None) => Some(a_on_enter),
            (None, Some(b_on_enter)) => Some(b_on_enter),
            // FIXME: right now, b overrides a. However, ideally, this would run both!
            (Some(_), Some(b_on_enter)) => Some(b_on_enter),
        };
        let on_leave = match (a.on_leave, b.on_leave) {
            (None, None) => None,
            (Some(a_on_leave), None) => Some(a_on_leave),
            (None, Some(b_on_leave)) => Some(b_on_leave),
            // FIXME: right now, b overrides a. However, ideally, this would run both!
            (Some(_), Some(b_on_leave)) => Some(b_on_leave),
        };

        TokenEvents {
            on_enter: on_enter,
            on_leave: on_leave,
        }
    }
    pub fn combine_from_optionals(a: Option<TokenEvents>, b: Option<TokenEvents>) -> TokenEvents {
        // If either one is None, then return the other one
        let Some(a) = a else {
            if let Some(b) = b {
                return b;
            } else {
                return TokenEvents::new_empty();
            }
        };
        let Some(b) = b else {
            return a;
        };

        // Otherwise, merge them
        TokenEvents::combine(a, b)
    }
}

#[derive(Debug)]
#[derive(Clone)]
pub struct Token {
    pub id: uuid::Uuid,
    pub template: TokenMatchTemplateMatcher,
    pub literal: Option<String>,
    pub matches: HashMap<String, TokenMatch>,

    pub events: TokenEvents,

    pub effects: Vec<TokenEffect>,

    pub next_id: Option<uuid::Uuid>,
    pub previous_id: Option<uuid::Uuid>,
    pub parent_id: Option<uuid::Uuid>,
    pub children_ids: Vec<uuid::Uuid>,
}

impl Token {
    pub fn next<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Option<&Box<Token>> {
        let Some(next_id) = self.next_id else {
            return None;
        };

        tokens_collection.get_by_id(next_id)
    }
    pub fn previous<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Option<&Box<Token>> {
        let Some(previous_id) = self.previous_id else {
            return None;
        };
        tokens_collection.get_by_id(previous_id)
    }
    pub fn parent<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Option<&Box<Token>> {
        let Some(parent_id) = self.parent_id else {
            return None;
        };
        tokens_collection.get_by_id(parent_id)
    }

    pub fn children<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Option<Vec<&Box<Token>>> {
        let mut children: Vec<&Box<Token>> = vec![];
        for child_id in &self.children_ids {
            let Some(child) = tokens_collection.get_by_id(*child_id) else {
                continue;
            };
            children.push(&child);
        }
        Some(children)
    }
    pub fn find_child<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        mut matcher: F
    ) -> Option<&Box<Token>> where F: FnMut(&Token) -> bool {
        let Some(children) = &self.children(tokens_collection) else {
            return None;
        };

        for child in children {
            if matcher(child) {
                return Some(child);
            };
        };
        None
    }
    pub fn find_children<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        mut matcher: F
    ) -> Vec<&Box<Token>> where F: FnMut(&Token) -> bool {
        let Some(children) = &self.children(tokens_collection) else {
            return vec![];
        };

        let mut matches = vec![];
        for child in children {
            if matcher(*child) {
                matches.push(*child);
            };
        };
        matches
    }
    pub fn deep_children<'a>(&'a self, tokens_collection: &'a TokensCollection, max_depth: Option<usize>) -> Option<Vec<&Box<Token>>> {
        let mut children: Vec<&Box<Token>> = vec![];
        for child_id in &self.children_ids {
            let Some(child) = tokens_collection.get_by_id(*child_id) else {
                continue;
            };
            children.push(&child);

            if max_depth == Some(0) {
                continue;
            };

            let next_max_depth = if let Some(max_depth) = max_depth {
                Some(max_depth - 1)
            } else {
                None
            };
            let Some(deep_children) = child.deep_children(tokens_collection, next_max_depth) else {
                continue;
            };
            children.extend(&deep_children);
        }
        Some(children)
    }
    pub fn find_deep_child<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        max_depth: Option<usize>,
        mut matcher: F
    ) -> Option<&Box<Token>> where F: FnMut(&Token) -> bool {
        let Some(children) = &self.deep_children(tokens_collection, max_depth) else {
            return None;
        };

        for child in children {
            if matcher(child) {
                return Some(child);
            };
        };
        None
    }
    pub fn find_deep_children<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        max_depth: Option<usize>,
        mut matcher: F
    ) -> Vec<&Box<Token>> where F: FnMut(&Token) -> bool {
        let Some(children) = &self.deep_children(tokens_collection, max_depth) else {
            return vec![];
        };

        let mut matches = vec![];
        for child in children {
            if matcher(*child) {
                matches.push(*child);
            };
        };
        matches
    }

    pub fn depth<'a>(&'a self, tokens_collection: &'a TokensCollection) -> usize {
        let mut pointer = Box::new(self);
        let mut depth = 0;
        loop {
            let Some(parent_id) = pointer.parent_id else {
                return depth;
            };
            let Some(parent) = tokens_collection.get_by_id(parent_id) else {
                return depth;
            };
            *pointer = parent;
            depth += 1;
        }
    }

    // When called, returns a list of all effects that are on tokens that is a direct or indirect
    // child of the given token, an ALSO are the first of a given type down any particular tree
    // leg.
    //
    // For example:
    // - Token 1 (effects=[])
    //   - Token 2 (effects=[A(1)])
    //     - Token 4 (effects=[A(9)])
    //   - Token 3 (effects=[A(5)])
    //
    // When called on Token 1, this function should only return A(1) and A(5), NOT A(9)!
    pub fn child_effects<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Vec<(&TokenEffect, Vec<&Box<Token>>)> {
        println!("=====\nCHILD EFFECTS\n=====");
        let mut child_effects_at_depths: Vec<(&TokenEffect, Vec<&Box<Token>>)> = vec![];

        let mut child_paths_to_traverse: Vec<Vec<&Box<Token>>> = self.children(tokens_collection)
            .unwrap()
            .iter()
            .map(|t| vec![*t])
            .collect();

        while let Some(child_path) = child_paths_to_traverse.pop() {
            let Some(&child) = child_path.last() else {
                break;
            };
            println!("-> CHILD PATH: {:?} effects={:?}", child_path.iter().map(|c| c.abbreviated_id()).collect::<Vec<String>>(), child.effects);
            let depth = child_path.len();

            for effect in &child.effects {
                // FIXME: this is a bad `.clone()`, I should be able to get away without doing this
                let child_effects_at_depths_clone = child_effects_at_depths.clone();

                // Look for any pre-existing effects that match the current effect
                let similar_pre_existing_effects: Vec<(usize, &(&TokenEffect, Vec<&Box<Token>>))> = child_effects_at_depths_clone
                    .iter()
                    .enumerate()
                    .filter(|(_, (child_effect, _))| {
                        std::mem::discriminant(effect) == std::mem::discriminant(child_effect)
                    })
                    // .map(|(child_effect_index, (child_effect, child_path))| {
                    //     (*child_effect, child_effect_index, child_path.len())
                    // })
                    .collect();

                println!("\tSIMILAR ALREADY STORED EFFECTS: {}", similar_pre_existing_effects.len());
                // If there are no pre existing similar effects, then definitely add this effect
                if similar_pre_existing_effects.len() == 0 {
                    child_effects_at_depths.push((effect, child_path.clone()));
                    continue;
                }

                let similar_pre_existing_effects_below: Vec<
                    &(usize, &(&TokenEffect, Vec<&Box<Token>>))
                > = similar_pre_existing_effects
                    .iter()
                    .filter(|(_, (_, pre_existing_effect_path))| {
                        println!(
                            "\tCOMPUTE IS EFFECT BELOW? pre_existing_effect_path={:?} child_path={:?}",
                            pre_existing_effect_path.iter().map(|t| t.abbreviated_id()).collect::<Vec<String>>(),
                            child_path.iter().map(|t| t.abbreviated_id()).collect::<Vec<String>>(),
                        );
                        for i in 0..std::cmp::min(pre_existing_effect_path.len(), child_path.len()) {
                            let pre_existing_effect_path_prefix = &pre_existing_effect_path[..i];
                            let child_path_prefix = &child_path[..i];

                            let vecs_non_equal = pre_existing_effect_path
                                .iter()
                                .zip(child_path_prefix.iter())
                                .find(|&(a, b)| a.id != b.id);
                            if vecs_non_equal.is_some() {
                                println!("\nEFFECT BELOW!");
                                return true;
                            }
                        }

                        let effect_is_below = pre_existing_effect_path.len() >= child_path.len();
                        println!("\nEFFECT BELOW? {}", effect_is_below);
                        effect_is_below
                    })
                    .collect();
                if similar_pre_existing_effects_below.len() > 0 {
                    let mut sorted_indexes = similar_pre_existing_effects_below
                        .iter()
                        .map(|(index, _)| index)
                        .collect::<Vec<&usize>>();
                    sorted_indexes.sort();

                    for index in sorted_indexes {
                        child_effects_at_depths.remove(*index);
                    }
                    child_effects_at_depths.push((effect, child_path.clone()));
                }
            }

            let Some(children) = child.children(tokens_collection) else {
                break;
            };
            for child in children {
                let mut next_path = child_path.clone();
                next_path.push(child);
                child_paths_to_traverse.push(next_path);
            }
        }

        child_effects_at_depths
    }
    pub fn find_child_effect<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        mut matcher: F,
    ) -> Option<(&TokenEffect, &Token)> where F: FnMut(&TokenEffect, &Token) -> bool {
        let effects = &self.child_effects(tokens_collection);
        println!("CHILD EFFECTS: {}", effects.len());
        for effect in effects {
            println!("-> {effect:?}");
        }
        let mut shallowest_matching_effect: Option<(&TokenEffect, &Token, usize)> = None;
        for (effect, token_path) in effects {
            let Some(token) = token_path.last() else {
                continue;
            };
            let depth = token_path.len();

            if !matcher(effect, token) {
                continue;
            }

            if shallowest_matching_effect.is_none() || matches!(shallowest_matching_effect, Some((_, _, old_depth)) if old_depth > depth) {
                shallowest_matching_effect = Some((effect, token, depth));
            }
        };

        match shallowest_matching_effect {
            Some((effect, token, _depth)) => Some((effect, token)),
            None => None,
        }
    }

    pub fn parent_effects<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Vec<(&TokenEffect, &Box<Token>)> {
        let mut pointer = Box::new(self);
        let mut parent_effects: Vec<(&TokenEffect, &Box<Token>)> = vec![];
        loop {
            let Some(parent_id) = pointer.parent_id else {
                return parent_effects;
            };
            let Some(parent) = tokens_collection.get_by_id(parent_id) else {
                return parent_effects;
            };
            for effect in &parent.effects {
                parent_effects.push((effect, parent));
            }
            *pointer = parent;
        }
    }
    pub fn find_parent_effect<'a>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        matcher: fn(e: &TokenEffect, t: &Token) -> bool,
    ) -> Option<(&TokenEffect, &Token)> {
        let effects = &self.parent_effects(tokens_collection);

        for (effect, token) in effects {
            if matcher(effect, token) {
                return Some((effect, token));
            };
        };
        None
    }

    pub fn find_current_or_parent_effect<'a>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        matcher: fn(e: &TokenEffect, t: &Token) -> bool,
    ) -> Option<(&TokenEffect, &Token)> {
        if let Some(effect) = self.effects.iter().find(|e| matcher(e, self)) {
            Some((effect, self))
        } else {
            self.find_parent_effect(tokens_collection, matcher)
        }
    }

    pub fn compute_offset<'a>(&'a self, tokens_collection: &'a mut TokensCollection) -> usize {
        tokens_collection.compute_offset(self.id)
    }

    pub fn abbreviated_id(&self) -> String {
        let token_id_str = format!("{}", self.id);
        format!(
            "{}..{}",
            &token_id_str[..3],
            &token_id_str[token_id_str.char_indices().nth_back(3).unwrap().0..],
        )
    }

    // When called, gets the literal text of this token and all of its decendants.
    pub fn stringify(&self, tokens_collection: &TokensCollection) -> String {
        let mut result = String::from("");
        let mut pointer_id = self.id;
        loop {
            let Some(pointer) = tokens_collection.get_by_id(pointer_id) else {
                break;
            };
            if let Some(literal_text) = &pointer.literal {
                result = format!("{}{}", result, literal_text);
            };
            if let Some(next_pointer_id) = pointer.next_id {
                // Only keep going if the next token is a CHILD of the token being stringified!
                if self.find_deep_child(tokens_collection, None, |child| {
                    child.id == next_pointer_id
                }).is_none() {
                    break;
                }

                pointer_id = next_pointer_id;
            } else {
                break;
            }
        }

        result
    }

    // Computes the "deep last child" of a token. This is defined as the last child's last child's
    // last child (and so on). In diagram form:
    //     self
    //      / \
    //     a   b
    //    /\   /\
    //   c  d e  f <-- `f` is the "deep last child"
    //
    // If the specified token doesn't have any children, this function returns `None`.
    pub fn deep_last_child_id(&self, token_collection: &TokensCollection) -> Option<uuid::Uuid> {
        let mut deep_last_referenced_child_id = if let Some(n) = self.children_ids.last() {
            Some(*n)
        } else { None };

        loop {
            let Some(deep_last_referenced_child_id_unwrapped) = deep_last_referenced_child_id else {
                break;
            };
            let Some(child_token) = token_collection.get_by_id(
                deep_last_referenced_child_id_unwrapped
            ) else {
                break;
            };
            if let Some(result) = child_token.children_ids.last() {
                deep_last_referenced_child_id = Some(*result);
            } else {
                break;
            }
        }

        deep_last_referenced_child_id
    }
    pub fn deep_last_child<'a>(&self, token_collection: &'a TokensCollection) -> Option<&'a Box<Token>> {
        if let Some(token_id) = self.deep_last_child_id(token_collection) {
            token_collection.get_by_id(token_id)
        } else {
            None
        }
    }

    // When called, reparses the token literal text into a token stream that matches the template
    // assigned to the token.
    //
    // This consumes the token to reparse as once it it reparsed, the token is removed from the
    // token collection
    pub fn reparse<'a>(self, token_collection: &'a mut TokensCollection) -> Result<Option<uuid::Uuid>, String> {
        let original_offset = self.compute_offset(token_collection);

        let token_offset = 0;

        // Create a clone of the token to modify in-memory
        let mut working_token = self.clone();
        let Some(mut working_new_text) = self.literal.clone() else {
            return Err(format!("Cannot reparse token {:?}, token.literal is None!", self.id));
        };
        let mut working_template = TokenMatchTemplate::new(
            vec![working_token.template.clone()],
        );
        println!("WORKING TOKEN ID: {}", working_token.id);

        //  1. attempt to parse
        //  2. If it won't fully parse, go up a level, and parse again
        //  3. If after going up a `regular_parse_max_upward_traverals` levels things still fail,
        //     then go up a level / reparse and be willing to accept a partial parse from now on
        let mut regular_parse_max_upward_traverals = Some(5);

        let mut match_iterations = 0;
        loop {
            println!("offset {}", token_offset);
            println!("parsing text: '{working_new_text}'");
            match working_template.consume_from_offset(
                &working_new_text,
                token_offset,
                working_token.previous_id,
                true,
                0,
                token_collection.token_match_templates_map.clone(),
            ) {
                Ok((match_status, _offset, last_token_id, child_ids, new_tokens)) => {
                    println!("MATCHED STATUS: {:?} => {:?} ({:?})", working_token.template, match_status, last_token_id);
                    println!("PARSED RESULT: {}", new_tokens.debug_token_tree_string());

                    if match_status != TokenParseStatus::FullParse {
                        if let Some(value) = regular_parse_max_upward_traverals {
                            // Once we've traversed as high up as we have to, then give up.
                            // This update must not yet be valid syntax and maybe will make more
                            // sense once it is further updated.
                            //
                            // NOTE: in theis case also, maybe take the part of the PartialParse
                            // that does match and replace that rather than just giving up
                            // outright?
                            if value == 0 {
                                println!("COULD NOT FIND FULL PARSE, APPLYING PARTIAL PARSE!");
                                token_collection.reset_caches_for_and_after(original_offset);

                                return working_token.replace_with_subtree(
                                    token_collection,
                                    new_tokens,
                                    child_ids,
                                );
                            }

                            // Make sure there is a parent to traverse upwards to:
                            if let Some(parent) = working_token.parent(token_collection) {
                                // Still at least one more iteration to go!
                                regular_parse_max_upward_traverals = Some(value-1);

                                match_iterations += 1;
                                working_template = TokenMatchTemplate::new(
                                    vec![parent.template.clone()],
                                );
                                working_token = *parent.clone();
                                working_new_text = working_token.stringify(token_collection);
                                continue;
                            };
                        }
                    }

                    token_collection.reset_caches_for_and_after(original_offset);

                    // Once the parse has completed successfully, replace the working token with
                    // the new tokens that were the result of the parse
                    return working_token.replace_with_subtree(
                        token_collection,
                        new_tokens,
                        child_ids,
                    );
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }
    
    // When called, removes `self` from the token stream, and inserts `child_ids` in its
    // place. This function handles forming the links between tokens so that the final parsed token
    // structure is still valid.
    //
    // Note that this consumes the token calling it as this token is removed as part of this
    // operation.
    pub fn replace_with_subtree<'a>(
        self,
        token_collection: &'a mut TokensCollection,
        new_tokens: TokensCollection,
        child_ids: Vec<uuid::Uuid>,
    ) -> Result<Option<uuid::Uuid>, String> {
        // Clear all caches of data at or after this token
        let original_offset = self.compute_offset(token_collection);
        token_collection.reset_caches_for_and_after(original_offset);
        // println!("SELF ID: {:?}", self.id);

        let first_child_id = if let Some(first_root_node_id) = child_ids.first() {
            Some(*first_root_node_id)
        } else {
            None
        };
        // println!("FIRST CHILD ID: {first_child_id:?}");
        let Some(first_child_id) = first_child_id else {
            return Ok(None);
        };

        let deep_last_referenced_child_id = {
            if let Some(final_root_node) = new_tokens.get_final_node() {
                let final_root_node_id = final_root_node.id.clone();
                Some(final_root_node_id)
            } else {
                None
            }
        };

        // Substitute in the new subtree into where the old subtree went
        for new_token in new_tokens.tokens {
            // println!("ADD NEW TOKEN! {:?}", new_token);
            token_collection.push(new_token);
        };

        // Finally, link the new token tree into the pre-existing token tree:
        //
        //                              (old parent)
        //                                 ||   /\
        //                               F ||   || E
        //                                 \/   ||
        //                         |'''''''''''''''''''''''''''|
        // (old previous) -- A --> first_child_id..last_child_id
        //               <-- B --                     / | \
        //                                           / / \ \
        //                                          1 2  3 /\
        //                                                /  \
        //                                               4    |
        //                                                    |
        //                           deep_last_referenced_child_id -- C --> (old's deep last child's next)
        //                                                         <-- D --

        // A:
        if let Some(working_token_previous_id) = self.previous_id {
            token_collection.get_by_id_mut(working_token_previous_id, |working_token_previous| {
                // println!("A: {}.next_id = {:?}", working_token_previous.abbreviated_id(), Some(first_child_id));
                working_token_previous.next_id = Some(first_child_id);
            });
        }
        // B:
        token_collection.get_by_id_mut(first_child_id, |first_child| {
            // println!("B: {}.previous_id = {:?}", first_child.abbreviated_id(), self.previous_id);
            first_child.previous_id = self.previous_id;
        });

        if let Some(deep_last_referenced_child_id) = deep_last_referenced_child_id {
            let working_token_deep_last_child_next_id = self
                .deep_last_child(token_collection)
                .map(|n| n.next_id)
                .unwrap_or(None);

            // If at the deep last child doesn't exist (ie, maybe this token
            // doesn't have children), then use the next of the working token
            // instead
            let working_token_next_value_id = working_token_deep_last_child_next_id.or(self.next_id);
            // C:
            token_collection.get_by_id_mut(deep_last_referenced_child_id, |deep_last_child| {
                // println!("{:?} {:?}", self.next_id, next_id);
                // println!("C: {}.next_id = {:?}", deep_last_child.abbreviated_id(), working_token_next_value_id);
                deep_last_child.next_id = working_token_next_value_id;
            });
            // D:
            if let Some(working_token_next_value_id) = working_token_next_value_id {
                token_collection.get_by_id_mut(working_token_next_value_id, |working_token_next_value| {
                    // println!("D: {}.previous_id = {:?}", working_token_next_value.abbreviated_id(), Some(deep_last_referenced_child_id));
                    working_token_next_value.previous_id = Some(deep_last_referenced_child_id);
                });
            }
        }

        // E:
        for child_id in &child_ids {
            token_collection.get_by_id_mut(*child_id, |child| {
                // println!("E: {}.parent_id = {:?}", child.abbreviated_id(), self.parent_id);
                child.parent_id = self.parent_id;
            });
        };
        // F:
        if let Some(working_token_parent_id) = self.parent_id {
            token_collection.get_by_id_mut(working_token_parent_id, |working_token_parent| {
                for child_id in &child_ids {
                    // println!("F: {}.children_ids.push({:?})", working_token_parent.abbreviated_id(), *child_id);
                    working_token_parent.children_ids.push(*child_id);
                }
            });
        }


        // Remove all tokens in the subtree underneath the matching working token
        let subtree_children_ids = match self.deep_children(token_collection, None) {
            Some(subtree_children) => {
                subtree_children.iter().map(|t| t.id).collect()
            },
            _ => vec![],
        };
        for child_id in subtree_children_ids {
            // println!("-> REMOVE TOKEN! {:?}", child_id);
            token_collection.remove(child_id);
        };

        // println!("REMOVE TOKEN! {:?}", self.id);
        token_collection.remove(self.id);

        // println!("AFTER INSERT: ----\n{}\n-------\n\n", token_collection.stringify());
        // println!("AFTER INSERT: ----\n{}\n-------\n\n", token_collection.debug_stringify_highlight(0, 0));
        println!("AFTER INSERT: ----\n{}\n-------\n\n", token_collection.debug_token_tree_string());
        Ok(Some(first_child_id))
    }

    // When called, locates the containing lexical scope, and returns a reference to the effect
    // that defines the scope and token that effect is on, or None if this token is not in a scope
    pub fn get_containing_lexical_scope<'a>(
        &'a self,
        token_collection: &'a TokensCollection,
    ) -> Option<(&TokenEffect, &Token)> {
        self.find_current_or_parent_effect(
            token_collection,
            |e, _| matches!(e, TokenEffect::DeclareLexicalScope),
        )
    }

    // When called on a token that contains a DeclareIdentifier effect, work up scope
    // by scope until a `DeclareExpression` containing the same `DeclareIdentifier` is found.
    pub fn go_to_definition<'a>(
        &'a self,
        token_collection: &'a TokensCollection,
    ) -> Result<Option<((&TokenEffect, &Token), (TokenEffect, Token))>, String> {
        let Some((declare_identifier_effect, _)) = self.find_current_or_parent_effect(
            token_collection,
            |e, _| matches!(e, TokenEffect::DeclareIdentifier(_)),
        ) else {
            return Err(format!("Cannot find DeclareIdentifier effect in current or any parent token of {}!", self.id));
        };

        // 1. Find the containing lexical scope
        // 2. Look for `DeclareAssignmentContainer`s in this lexical scope
        // 3. Go up a lexical scope and repeat

        let mut working_token = self;
        let mut last_working_scope_token_id: Option<uuid::Uuid> = None;
        while let Some((working_scope, working_scope_token)) = working_token.get_containing_lexical_scope(token_collection) {
            println!("SCOPE: {working_scope:?}");

            if last_working_scope_token_id == Some(working_scope_token.id) {
                break;
            }
            last_working_scope_token_id = Some(working_scope_token.id);

            // TODO: api idea:
            // self.find_effect_pattern(
            //     token_collection,
            //     TokenEffectSelector::find_children("foo", TokenEffectSelector::DeclareAssignmentContainer, vec![
            //         TokenEffectSelector::find("bar", TokenEffect::DeclareIdentifier("myvar"))
            //         TokenEffectSelector::find("baz", TokenEffect::DeclareExpression(_))
            //     ]),
            // );

            // NOTE: The pattern that is being searched for is a DeclareAssignmentContainer with a
            // DeclareIdentifier inside of it
            let mut identifier_effect_result: Option<(TokenEffect, Token)> = None;
            let Some(assignment_container_result) = working_scope_token.find_child_effect(token_collection, |effect, token| {
                // println!("ASSIGNMENT? {effect:?} {}", token.id);
                // Prune legs of the search where DeclareAssignmentContainer doesn't even exist
                if !matches!(effect, TokenEffect::DeclareAssignmentContainer) {
                    return false;
                }
                // println!("FOUND DeclareAssignmentContainer: {token:?}");

                let Some((identifier_effect, identifier_token)) = token.find_child_effect(
                    token_collection,
                    |effect, _| {
                        println!("EFFECT? {effect:?} {declare_identifier_effect:?}");
                        matches!(effect, declare_identifier_effect)
                    }
                ) else {
                    return false;
                };

                identifier_effect_result = Some((identifier_effect.clone(), identifier_token.clone()));
                return true;
            }) else {
                // Nothing was found in this scope, so move up to the previous scope and try again!
                let Some(parent_token) = working_scope_token.parent(token_collection) else {
                    return Ok(None);
                };
                working_token = parent_token;
                continue;
            };

            // 
            return Ok(Some(
                (assignment_container_result, identifier_effect_result.unwrap())
            ))
        }
        Ok(None)
    }
}

#[cfg(test)]
mod test_token {
    use crate::{TokenMatchTemplate, TokenMatchTemplateMap};
    use crate::TokenParseStatus;
    use crate::Document;
    use regex::Regex;
    use std::rc::Rc; 
    use super::*;

    // This mini language is either:
    // - A single `1`
    // - Between 1 to 3 `1`s, followed by a `2`
    fn initialize_mini_language_twelve() -> (TokenMatchTemplateMap, TokenMatchTemplate) {
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

        (token_match_templates_map.clone(), token_match_templates_map.get("All").unwrap().clone())
    }

    // This mini language implements a four function math expression parser with parenthesis
    // for grouping.
    fn initialize_mini_language_math() -> (TokenMatchTemplateMap, TokenMatchTemplate) {
        let mut token_match_templates_map = HashMap::new();
        token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::any(vec![
                TokenMatchTemplateMatcher::reference("Addition"),
                TokenMatchTemplateMatcher::reference("Subtraction"),
                TokenMatchTemplateMatcher::reference("Multiplication"),
                TokenMatchTemplateMatcher::reference("Division"),
                TokenMatchTemplateMatcher::reference("Integer"),
            ]),
        ]));
        token_match_templates_map.insert("Expression", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::any(vec![
                TokenMatchTemplateMatcher::reference("Integer"),
                TokenMatchTemplateMatcher::reference("Group"),
            ]),
        ]));
        token_match_templates_map.insert("Group", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::raw("("),
            TokenMatchTemplateMatcher::any(vec![
                TokenMatchTemplateMatcher::reference("Addition"),
                TokenMatchTemplateMatcher::reference("Subtraction"),
                TokenMatchTemplateMatcher::reference("Multiplication"),
                TokenMatchTemplateMatcher::reference("Division"),
                TokenMatchTemplateMatcher::reference("Integer"),
            ]),
            TokenMatchTemplateMatcher::raw(")"),
        ]));
        token_match_templates_map.insert("Addition", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::reference("Expression"),
            TokenMatchTemplateMatcher::raw("+"),
            TokenMatchTemplateMatcher::reference("Expression"),
        ]));
        token_match_templates_map.insert("Subtraction", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::reference("Expression"),
            TokenMatchTemplateMatcher::raw("-"),
            TokenMatchTemplateMatcher::reference("Expression"),
        ]));
        token_match_templates_map.insert("Multiplication", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::reference("Expression"),
            TokenMatchTemplateMatcher::raw("*"),
            TokenMatchTemplateMatcher::reference("Expression"),
        ]));
        token_match_templates_map.insert("Division", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::reference("Expression"),
            TokenMatchTemplateMatcher::raw("/"),
            TokenMatchTemplateMatcher::reference("Expression"),
        ]));
        token_match_templates_map.insert("Integer", TokenMatchTemplate::new(vec![
            TokenMatchTemplateMatcher::regex(
                Regex::new(r"^(?<value>-?[0-9]+)").unwrap(),
            ),
        ]));

        (token_match_templates_map.clone(), token_match_templates_map.get("All").unwrap().clone())
    }

    mod test_token_replace_with_subtree {
        use super::*;

        #[test]
        fn is_able_to_replace_flat_subtree_with_flat_tokencollection() {
            let mut document = Document::new_from_literal_with_token_lengths(
                "123456",
                vec![1, 1, 1, 1, 1, 1],
            );
            let token_collection = document.tokens_mut();

            let Some((token, _)) = token_collection.get_by_offset(3) else {
                panic!("Unable to call get_by_offset in is_able_to_replace_flat_subtree_with_flat_tokencollection!");
            };

            let mut new_document = Document::new_from_literal_with_token_lengths(
                "abcdef",
                vec![1, 1, 1, 1, 1, 1],
            );
            let new_token_collection = new_document.tokens_owned();
            // NOTE: because this tokencollection is flat, all tokens are children
            let top_level_child_ids = new_token_collection.tokens.iter().map(|t| t.id).collect();

            println!("PRE: {}", token_collection.debug_token_tree_string());

            let result = token.clone().replace_with_subtree(
                token_collection,
                new_token_collection,
                top_level_child_ids,
            );

            println!("RESULT: {result:?}");

            // Make sure the new token subtree contains the right data
            assert_eq!(token_collection.stringify(), "12abcdef456");
        }

        #[test]
        fn is_able_to_replace_deep_subtree_with_flat_tokencollection() {
            let (template_map, all_template) = initialize_mini_language_twelve();
            let template_map_rc = Rc::new(template_map);

            // Get a few subranges to make sure they generate the right data
            let mut document = {
                let result = all_template.consume_from_start("1112", false, template_map_rc.clone()).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            let token_collection = document.tokens_mut();

            let Some((token, _)) = token_collection.get_by_offset(2) else {
                panic!("Unable to call get_by_offset in is_able_to_replace_deep_subtree_with_flat_tokencollection!");
            };

            let mut new_document = Document::new_from_literal_with_token_lengths(
                "abcdef",
                vec![1, 1, 1, 1, 1, 1],
            );
            let new_token_collection = new_document.tokens_owned();
            // NOTE: because this tokencollection is flat, all tokens are children
            let top_level_child_ids = new_token_collection.tokens.iter().map(|t| t.id).collect();

            println!("PRE: {}", token_collection.debug_token_tree_string());

            let result = token.clone().replace_with_subtree(
                token_collection,
                new_token_collection,
                top_level_child_ids,
            );

            println!("RESULT: {result:?}");

            // Make sure the new token subtree contains the right data
            assert_eq!(token_collection.stringify(), "1abcdef12");
        }

        #[test]
        fn is_able_to_replace_deep_subtree_with_deep_tokencollection() {
            let (template_map, all_template) = initialize_mini_language_twelve();
            let template_map_rc = Rc::new(template_map);

            let mut document = {
                let result = all_template.consume_from_start("1112", false, template_map_rc.clone()).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            let token_collection = document.tokens_mut();

            let Some((token, _)) = token_collection.get_by_offset(2) else {
                panic!("Unable to call get_by_offset in is_able_to_replace_deep_subtree_with_flat_tokencollection!");
            };

            let result = all_template.consume_from_start("12", false, template_map_rc.clone()).unwrap();
            let new_token_collection = result.4;
            let top_level_child_ids = result.3;

            println!("PRE: {}", token_collection.debug_token_tree_string());

            let result = token.clone().replace_with_subtree(
                token_collection,
                new_token_collection,
                top_level_child_ids,
            );

            println!("RESULT: {result:?}");

            // Make sure the new token subtree contains the right data
            assert_eq!(token_collection.stringify(), "11212");
        }
    }

    mod test_token_reparse {
        use super::*;

        #[test]
        fn is_able_to_noop_reparse_a_changed_token() {
            let mut document = Document::new_from_literal_with_token_lengths(
                "123456",
                vec![1, 1, 1, 1, 1, 1],
            );
            let token_collection = document.tokens_mut();

            let token_id = {
                let Some((token, _)) = token_collection.get_by_offset(3) else {
                    panic!("Unable to call get_by_offset in is_able_to_noop_reparse_a_changed_token!");
                };
                token.id
            };

            // Change the token literal value
            token_collection.get_by_id_mut(token_id, |token| {
                token.literal = Some(String::from("XX"));
            });

            let Some(token) = token_collection.get_by_id(token_id) else {
                panic!("Cannot get token with id {token_id} in is_able_to_noop_reparse_a_changed_token!");
            };

            println!("PRE: {}", token_collection.debug_token_tree_string());

            // Before reparsing, make sure that the token collection 
            assert_eq!(token_collection.stringify(), "12XX456");

            // Force reparse the token - this shouldn't do anything, since the token template is
            // TokenMatchTemplateMatcher::Skipped
            token.clone().reparse(token_collection).unwrap();

            println!("POST: {}", token_collection.debug_token_tree_string());

            // Make sure the updated token collection still parses correctly
            assert_eq!(token_collection.stringify(), "12XX456");
        }

        #[test]
        fn is_able_to_reparse_token_in_deep_document() {
            let (template_map, all_template) = initialize_mini_language_math();
            let template_map_rc = Rc::new(template_map);

            let mut document = {
                let result = all_template.consume_from_start("(1+1)*5", false, template_map_rc.clone()).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            let token_collection = document.tokens_mut();

            let token_id = {
                let Some((token, _)) = token_collection.get_by_offset(4) else {
                    panic!("Unable to call get_by_offset in is_able_to_reparse_token_in_deep_document!");
                };
                token.id
            };

            // Change the token literal value
            token_collection.get_by_id_mut(token_id, |token| {
                token.literal = Some(String::from("(4/3)"));
            });

            let Some(token) = token_collection.get_by_id(token_id) else {
                panic!("Cannot get token with id {token_id} in is_able_to_reparse_token_in_deep_document!");
            };

            println!("PRE: {}", token_collection.debug_token_tree_string());

            // Before reparsing, make sure that the token collection 
            assert_eq!(token_collection.stringify(), "(1+(4/3))*5");

            // Force reparse the token - this shouldn't do anything, since the token template is
            // TokenMatchTemplateMatcher::Skipped
            token.clone().reparse(token_collection).unwrap();

            println!("POST: {}", token_collection.debug_token_tree_string());

            // Make sure the updated token collection still parses correctly
            assert_eq!(token_collection.stringify(), "(1+(4/3))*5");
        }

        #[test]
        fn is_able_to_append_bogus_char_and_parsing_not_fail() {
            let (template_map, all_template) = initialize_mini_language_math();
            let template_map_rc = Rc::new(template_map);

            let mut document = {
                let result = all_template.consume_from_start("(1+1)*5", false, template_map_rc.clone()).unwrap();
                Document::new_from_tokenscollection(Box::new(result.4))
            };
            let token_collection = document.tokens_mut();

            let token_id = {
                let Some((token, _)) = token_collection.get_by_offset(3) else {
                    panic!("Unable to call get_by_offset in is_able_to_append_bogus_char_and_parsing_not_fail!");
                };
                token.id
            };

            // Change the token literal value - add an X in there, which the math language doesn't
            // know how to parse
            token_collection.get_by_id_mut(token_id, |token| {
                token.literal = Some(String::from("+X"));
            });

            let Some(token) = token_collection.get_by_id(token_id) else {
                panic!("Cannot get token with id {token_id} in is_able_to_append_bogus_char_and_parsing_not_fail!");
            };

            println!("PRE: {}", token_collection.debug_token_tree_string());

            // Before reparsing, make sure that the token collection 
            assert_eq!(token_collection.stringify(), "(1+X1)*5");

            // Force reparse the token - this shouldn't do anything, since the token template is
            // TokenMatchTemplateMatcher::Skipped
            token.clone().reparse(token_collection).unwrap();

            println!("POST: {}", token_collection.debug_token_tree_string());

            // Make sure the updated token collection still parses correctly
            assert_eq!(token_collection.stringify(), "(1+X1)*5");
        }
    }
}
