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
            if offset != 0 && (offset + pointer_length) >= input_offset {
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

    pub fn reset_caches_for_and_after(&mut self, token_id: uuid::Uuid) -> bool {
        // Walk through the token collection, removing all cached elements at and after `token_id`
        // from `self.offset_cache`
        // self.offset_cache.borrow_mut().clear();

        let mut pointer_id = token_id;
        loop {
            let Some(pointer) = self.get_by_id(pointer_id) else {
                break;
            };

            self.offset_cache.borrow_mut().remove(&pointer_id);

            let mut should_break = true;
            if let Some(next_pointer_id) = pointer.next_id {
                pointer_id = next_pointer_id;
                should_break = false;
            };

            if should_break {
                break;
            };
        };

        // Delete the whole range of data starting at `offset` and going all the way to the end of
        // `tokens_by_start_offset_cache`.
        self.tokens_by_start_offset_cache.borrow_mut().clear();
        // {
        //     let offset = self.compute_offset(token_id);
        //     let maximum_cached_offset = {
        //         let result_range = self.tokens_by_start_offset_cache
        //             .borrow()
        //             .iter()
        //             .map(|(range, _)| range)
        //             .fold(
        //                 0..offset,
        //                 |rangea, rangeb| {
        //                     let rangea_end = rangea.end;
        //                     if rangea_end.max(rangeb.end) == rangea_end {
        //                         rangea
        //                     } else {
        //                         rangeb.clone()
        //                     }
        //                 },
        //             );
        //         result_range.end
        //     };
        //
        //     if maximum_cached_offset > offset {
        //         self.tokens_by_start_offset_cache.borrow_mut().remove(offset..maximum_cached_offset);
        //     }
        // };

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
        let Some(old_token) = self.get_by_id(token_id) else {
            return Err(format!("Cannot find token with id {}", token_id));
        };
        let token_offset = 0;

        // Create a clone of the token to modify in-memory
        let mut working_token = old_token.clone();
        working_token.literal = Some(new_text.clone());

        // Clear all caches of data at or after this token
        self.reset_caches_for_and_after(working_token.id);

        let mut working_template = TokenMatchTemplate::new(
            vec![working_token.template.clone()],
        );

        //  1. attempt to parse
        //  2. If it won't fully parse, go up a level, and parse again
        //  3. If after going up a `regular_parse_max_upward_traverals` levels things still fail,
        //     then go up a level / reparse and be willing to accept a partial parse from now on
        let mut regular_parse_max_upward_traverals = Some(5);

        let mut match_iterations = 0;
        loop {
            println!("offset {}", token_offset);
            match working_template.consume_from_offset(
                &new_text,
                token_offset,
                working_token.previous_id,
                true,
                0,
                self.token_match_templates_map.clone(),
            ) {
                Ok((match_status, _offset, last_token_id, child_ids, new_tokens)) => {
                    println!("MATCHED STATUS: {:?} => {:?} ({:?})", working_token.template, match_status, last_token_id);

                    if match_status != TokenParseStatus::FullParse {
                        if let Some(value) = regular_parse_max_upward_traverals {
                            // Once we've traversed as high up as we have to, then be a little more
                            // flexible in what we can accept. Be willing to accept a partial parse to get
                            // this operation over with
                            if value > 0 {
                                // Make sure there is a parent to traverse upwards to:
                                if let Some(parent) = working_token.parent(&self) {
                                    // Still at least one more iteration to go!
                                    regular_parse_max_upward_traverals = Some(value-1);

                                    match_iterations += 1;
                                    working_template = TokenMatchTemplate::new(
                                        vec![parent.template.clone()],
                                    );
                                    working_token = parent.clone();
                                    continue;
                                };
                            }
                        }
                    }

                    let first_child_id = if let Some(first_root_node_id) = child_ids.first() {
                        Some(*first_root_node_id)
                    } else {
                        None
                    };
                    println!("FIRST CHILD ID: {first_child_id:?}");
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
                    println!("DEEP LAST REFERENCED CHILD ID: {deep_last_referenced_child_id:?}");

                    // Remove all tokens in the subtree underneath the matching working token
                    let subtree_children_ids = match working_token.deep_children(self, match_iterations) {
                        Some(subtree_children) => {
                            subtree_children.iter().map(|t| t.id).collect()
                        },
                        _ => vec![],
                    };
                    // for child_id in subtree_children_ids {
                    //     println!("-> REMOVE TOKEN! {:?}", child_id);
                    //     self.remove(child_id);
                    // };
                    println!("REMOVE TOKEN! {:?}", working_token.id);
                    self.remove(working_token.id);

                    // Substitute in the new subtree into where the old subtree went
                    for new_token in new_tokens.tokens {
                        println!("ADD NEW TOKEN! {:?}", new_token);
                        self.push(new_token);
                    };

                    // Figure out the token that is the final "next" token in the new token tree
                    // let mut deep_last_referenced_child_id = if let Some(n) = child_ids.last() {
                    //     Some(*n)
                    // } else { None };
                    // loop {
                    //     let Some(deep_last_referenced_child_id_unwrapped) = deep_last_referenced_child_id else {
                    //         break;
                    //     };
                    //     let Some(child_token) = self.get_by_id(
                    //         deep_last_referenced_child_id_unwrapped
                    //     ) else {
                    //         break;
                    //     };
                    //     if let Some(result) = child_token.children_ids.last() {
                    //         deep_last_referenced_child_id = Some(*result);
                    //     } else {
                    //         break;
                    //     }
                    // }

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
                    //                           deep_last_referenced_child_id -- C --> (old next)
                    //                                                         <-- D --

                    // A:
                    if let Some(working_token_previous_id) = working_token.previous_id {
                        self.get_by_id_mut(working_token_previous_id, |working_token_previous| {
                            working_token_previous.next_id = Some(first_child_id);
                        });
                    }
                    // B:
                    self.get_by_id_mut(first_child_id, |first_child| {
                        first_child.previous_id = working_token.previous_id;
                    });

                    if let Some(deep_last_referenced_child_id) = deep_last_referenced_child_id {
                        // C:
                        self.get_by_id_mut(deep_last_referenced_child_id, |deep_last_child| {
                            deep_last_child.next_id = working_token.next_id;
                        });
                        // D:
                        if let Some(working_token_next_id) = working_token.next_id {
                            self.get_by_id_mut(working_token_next_id, |working_token_next| {
                                working_token_next.previous_id = Some(deep_last_referenced_child_id);
                            });
                        }
                    }

                    // E:
                    for child_id in &child_ids {
                        self.get_by_id_mut(*child_id, |child| {
                            child.parent_id = working_token.parent_id;
                        });
                    };
                    // F:
                    if let Some(working_token_parent_id) = working_token.parent_id {
                        self.get_by_id_mut(working_token_parent_id, |working_token_parent| {
                            for child_id in &child_ids {
                                working_token_parent.children_ids.retain(|parent_child_id| *parent_child_id != *child_id);
                            }
                        });
                    }

                    return Ok(Some(first_child_id))
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }

    // When called, removes a token from the token tree. When removed, all token links are updated
    // so that this removed token is now omitted.
    //
    // If one attempts to remove a non-leaf token, this function returns an `Err`
    pub fn remove_leaf(&mut self, token_id: uuid::Uuid) -> Result<bool, String> {
        let old_token_data = {
            let Some(old_token) = self.get_by_id(token_id) else {
                return Err(format!("Cannot find token {}", token_id));
            };

            // Get the deepest last child id in the token subtree
            let mut deep_last_referenced_child_id = old_token.children_ids.last();
            loop {
                let Some(deep_last_referenced_child_id_unwrapped) = deep_last_referenced_child_id else {
                    break;
                };
                let Some(child_token) = self.get_by_id(
                    *deep_last_referenced_child_id_unwrapped
                ) else {
                    break;
                };
                if let Some(result) = child_token.children_ids.last() {
                    deep_last_referenced_child_id = Some(result);
                } else {
                    break;
                }
            }

            Ok((
                old_token.next_id,
                old_token.previous_id,
                old_token.parent_id,
                deep_last_referenced_child_id,
            ))
        };

        match old_token_data {
            Ok((
                old_token_next_id,
                old_token_previous_id,
                old_token_parent_id,
                deep_last_referenced_child_id,
            )) => {
                // Update the pointer on the child AFTER old_token that points to the final deep child
                if let Some(deep_last_referenced_child_id) = deep_last_referenced_child_id {
                   if let Some(last_deep_child) = self.get_by_id(*deep_last_referenced_child_id) {
                        if let Some(token_after_old_token_id) = last_deep_child.next_id {
                            self.get_by_id_mut(token_after_old_token_id, |token_after_old_token| {
                                token_after_old_token.previous_id = old_token_previous_id;
                            })
                        }
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
                self.reset_caches_for_and_after(token_id);

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
    DeclareExpression(String),
    DeclareIdentifier(String),
    // DeclareContainer,
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
    pub fn deep_children<'a>(&'a self, tokens_collection: &'a TokensCollection, max_depth: usize) -> Option<Vec<&Box<Token>>> {
        let mut children: Vec<&Box<Token>> = vec![];
        for child_id in &self.children_ids {
            let Some(child) = tokens_collection.get_by_id(*child_id) else {
                continue;
            };
            children.push(&child);

            if max_depth == 0 {
                continue;
            };

            let Some(deep_children) = child.deep_children(tokens_collection, max_depth-1) else {
                continue;
            };
            children.extend(&deep_children);
        }
        Some(children)
    }
    pub fn find_deep_child<'a, F>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        max_depth: usize,
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
        max_depth: usize,
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

    pub fn child_effects<'a>(&'a self, tokens_collection: &'a TokensCollection) -> Option<Vec<&TokenEffect>> {
        let mut child_effects: Vec<&TokenEffect> = vec![];
        let Some(children) = &self.children(tokens_collection) else {
            return Some(child_effects);
        };
        for child in children {
            for effect in &child.effects {
                child_effects.push(effect);
            }
        }
        Some(child_effects)
    }
    pub fn find_child_effect<'a>(
        &'a self,
        tokens_collection: &'a TokensCollection,
        matcher: fn(e: &TokenEffect) -> bool,
    ) -> Option<&TokenEffect> {
        let Some(effects) = &self.child_effects(tokens_collection) else {
            return None;
        };

        for effect in effects {
            if matcher(effect) {
                return Some(effect);
            };
        };
        None
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
}
