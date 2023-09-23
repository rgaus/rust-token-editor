use std::collections::HashMap;
use uuid::Uuid;

use crate::token::*;


// The max number of times that a "forever" type token can ever repeat
//
// This should be set to something very high that is unlikely to ever be reached in
// practical use, but low enough to catch any seemligly infinitely recursing cases
// in a reasonable amount of time.
const TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH: usize = 99;

#[derive(Debug)]
#[derive(PartialEq)] 
pub enum TokenParseStatus {
    Failed,
    FullParse,
    PartialParse(usize /* chars_matched */, Option<usize> /* first_non_parsable_char_index */),
}

#[derive(Debug)]
#[derive(Clone)]
pub struct TokenMatchTemplate {
    matcher: Vec<TokenMatchTemplateMatcher>,
    events: Option<TokenEvents>,
}

impl TokenMatchTemplate {
    pub fn new(matcher: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher, events: None }
    }
    pub fn new_with_events(matcher: Vec<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher, events: Some(events) }
    }

    pub fn consume_from_start(
        &self,
        input: &str,
        store_non_parsable_chars: bool,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(TokenParseStatus, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensCollection), String> {
        self.consume_from_offset(input, 0, None, store_non_parsable_chars, 0, token_match_templates_map)
    }

    pub fn consume_from_offset(
        &self,
        input: &str,
        initial_offset: usize,
        initial_last_token_id: Option<uuid::Uuid>,
        store_non_parsable_chars: bool,
        depth: usize,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(TokenParseStatus, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensCollection), String> {
        let mut tokens = TokensCollection::new_empty();
        let mut offset = initial_offset;
        let mut child_ids: Vec<uuid::Uuid> = vec![];

        if depth > TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH {
            return Ok((TokenParseStatus::Failed, offset, initial_last_token_id, child_ids, tokens))
        };

        // An empty input should fail to parse
        if input.len() == 0 {
            return Ok((TokenParseStatus::Failed, offset, initial_last_token_id, child_ids, tokens))
        };

        let mut depth_spaces = String::from("");
        for _ in 0..depth {
            depth_spaces = format!("{}| ", depth_spaces);
            // depth_spaces = format!("{}  ", depth_spaces);
        }

        let mut last_token_id: Option<uuid::Uuid> = initial_last_token_id;
        let mut next_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();
        let mut previous_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();

        let mut matched_token_count = 0;
        let mut matched_partial = false;
        let mut first_non_parsable_char_index: Option<usize> = None;

        for template_matcher in &self.matcher {
            if offset > input.len()-1 {
                // When reaching the end of the input after parsing the first token and before
                // parsing the last token, that makes a partial match
                if matched_token_count > 0 && matched_token_count != self.matcher.len() {
                    matched_partial = true;
                };
                println!("{}EOF!", depth_spaces);
                break;
            };

            // When `store_non_parsable_chars` is enabled, attempt to look forward to find
            // a match if the match didn't work
            let should_look_for_non_parsable_chars = if offset == 0 {
                // If at the very start, then matching characters at the start is fine so that
                // any bogus chars before the match are captured
                true
            } else if self.matcher.len() == 1 {
                // If there is only one token to be matched, then any characters before won't
                // force the system down a complicated branch since there is only one token
                // This is meant to hit in cases like an REF(identifier) containing a single REGEX
                true
            } else {
                // When not at the very start, look for characters only after the first token
                // parses. This ensures that the parser doesn't wrongly assume a branch matches
                // because it matched some non parsable stuff at the beginning of a match.
                offset != initial_offset
            };

            let offsetted_input = &input[offset..];
            let escaped_offsetted_input = format!("`{}`", offsetted_input.replace("\n", "\\n"));
            match template_matcher {
                TokenMatchTemplateMatcher::Raw(raw, events) => {
                    println!("{}RAW({}): {} {}", depth_spaces, raw, escaped_offsetted_input, offset);
                    let mut matched = offsetted_input.starts_with(raw);

                    // When `store_non_parsable_chars` is enabled, attempt to look forward to find
                    // a match if the match didn't work
                    if !matched && store_non_parsable_chars && should_look_for_non_parsable_chars {
                        let mut matched_at_index = None;
                        for index in offset..input.len() {
                            if input[index..].starts_with(raw) {
                                matched_at_index = Some(index);
                                // println!("MATCHED AT INDEX: {}", index);
                                break;
                            }
                        }

                        if let Some(index) = matched_at_index {
                            // Matched the token, but had to skip some characters to get there
                            println!("{}UNPARSABLE CHARS: offset={} index={} `{}`", depth_spaces, offset, index, &input[offset..index]);

                            // If a non parsable character hasn't been found yet in the stream,
                            // then note this one down as the first occurance.
                            //
                            // This data is used by the caller to weight different branches in the
                            // parse tree and figure out which one to pick
                            if first_non_parsable_char_index.is_none() {
                                first_non_parsable_char_index = Some(offset);
                            }

                            let mut new_token = Box::new(Token {
                                id: Uuid::new_v4(),
                                template: TokenMatchTemplateMatcher::Skipped,
                                literal: Some(String::from(&input[offset..index])),
                                matches: HashMap::new(),
                                effects: vec![],
                                events: TokenEvents::new_empty(),
                                next_id: None,
                                previous_id: None,
                                parent_id: None,
                                children_ids: vec![],
                            });
                            // if let Some(on_enter) = new_token.events.on_enter {
                            //     on_enter(&mut new_token);
                            // }

                            child_ids.push(new_token.id);

                            // println!("RW: {:?} <- {:?}", new_token.id, last_token_id);
                            previous_id_mapping.insert(new_token.id, last_token_id);
                            if let Some(last_token_id_unwrapped) = last_token_id {
                                // println!("RW: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                                next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                            }
                            last_token_id = Some(new_token.id);

                            tokens.push(new_token);

                            // If characters had to be skipped, it's definitely a partial match
                            matched_partial = true;

                            // Update the offset in the token stream to the place where the token
                            // was found
                            offset = index;
                            matched = true;
                        } else {
                            // Did not match the token!
                            break;
                        }
                    }

                    if !matched {
                        break;
                    }

                    let mut new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: Some(String::from(*raw)),
                        matches: HashMap::new(),
                        effects: vec![],
                        events: match events {
                            Some(events) => events.clone(),
                            None => TokenEvents::new_empty(),
                        },
                        next_id: None,
                        previous_id: None,
                        parent_id: None,
                        children_ids: vec![],
                    });
                    if let Some(on_enter) = new_token.events.on_enter {
                        on_enter(&mut new_token);
                    }

                    child_ids.push(new_token.id);

                    // println!("RW: {:?} <- {:?}", new_token.id, last_token_id);
                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        // println!("RW: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }
                    last_token_id = Some(new_token.id);

                    tokens.push(new_token);

                    offset += raw.len();
                }
                TokenMatchTemplateMatcher::Reference(reference_name, events) => {
                    println!("{}REF({}): {} {}", depth_spaces, reference_name, escaped_offsetted_input, offset);
                    let Some(referenced_template) = token_match_templates_map.get(reference_name) else {
                        return Err(format!("Unknown reference name {} found!", reference_name))
                    };

                    let Ok((
                        referenced_parse_status,
                        referenced_template_offset,
                        _referenced_last_token_id,
                        referenced_child_ids,
                        referenced_template_tokens,
                    )) = referenced_template.consume_from_offset(
                        input,
                        offset,
                        last_token_id,
                        store_non_parsable_chars,
                        depth + 1,
                        token_match_templates_map,
                    ) else {
                        break;
                    };
                    println!("{}`-- (referenced_parse_status={:?})", depth_spaces, referenced_parse_status);
                    if referenced_parse_status == TokenParseStatus::Failed {
                        break;
                    };
                    // If this reference matched partially, then this next match also is partial
                    if let TokenParseStatus::PartialParse(_, first_non_parsable_char_index_temp) = referenced_parse_status {
                        if let Some(value) = first_non_parsable_char_index_temp {
                            first_non_parsable_char_index = Some(value);
                        }
                        matched_partial = true;
                    };

                    let mut new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: None,
                        matches: HashMap::new(),
                        effects: vec![],
                        events: TokenEvents::combine_from_optionals(
                            referenced_template.events.clone(),
                            events.clone(),
                        ),
                        next_id: None,
                        previous_id: None,
                        parent_id: None,
                        children_ids: vec![],
                    });
                    if let Some(on_enter) = new_token.events.on_enter {
                        on_enter(&mut new_token);
                    }

                    // Link the new token's next_id to the first child
                    if let Some(first_referenced_child_id) = referenced_child_ids.first() {
                        // println!("RF: {:?} -> {:?}", new_token.id, Some(*first_referenced_child_id));
                        next_id_mapping.insert(new_token.id, Some(*first_referenced_child_id));
                    }

                    // Link the new token with its previous token
                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        // println!("RF: {:?} <- {:?}", last_token_id_unwrapped, Some(new_token.id));
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }

                    // Find the deepest last child in the referenced token hierarchy
                    let mut deep_last_referenced_child_id = referenced_child_ids.last();
                    loop {
                        let Some(deep_last_referenced_child_id_unwrapped) = deep_last_referenced_child_id else {
                            break;
                        };
                        let Some(child_token) = referenced_template_tokens.get_by_id(
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
                    last_token_id = if let Some(deep_last_referenced_child_id) = deep_last_referenced_child_id {
                        // The next "last token" should be the last child
                        Some(*deep_last_referenced_child_id)
                    } else {
                        // The next "last token" should be the token that was just added
                        Some(new_token.id)
                    };

                    new_token.children_ids = referenced_child_ids;
                    child_ids.push(new_token.id);

                    let new_token_id = new_token.id;
                    tokens.push(new_token);

                    let mut temp_referenced_template_tokens_without_parents: Vec<Box<Token>> = vec![];
                    // Add all tokens to the tokens array that don't have parents
                    for token in referenced_template_tokens.tokens {
                        if token.parent_id == None {
                            temp_referenced_template_tokens_without_parents.push(token);
                            continue
                        };
                        tokens.push(token);
                    }
                    // Then add all the tokens with parents AFTER so that when `on_leave` is
                    // called, all the child tokens (that are already parented) will be present
                    // in `tokens` first
                    for mut token in temp_referenced_template_tokens_without_parents {
                        token.parent_id = Some(new_token_id);
                        if let Some(on_leave) = token.events.on_leave {
                            on_leave(&mut token, &mut tokens);
                        }
                        tokens.push(token);
                    }

                    offset = referenced_template_offset;
                }
                TokenMatchTemplateMatcher::Regex(re, negated_re, events) => {
                    println!("{}REGEX({:?}, {:?}): {} {}", depth_spaces, re, negated_re, escaped_offsetted_input, offset);

                    let mut matched_captures: Option<regex::Captures> = None;

                    // ref: https://stackoverflow.com/a/39239614/4115328
                    if let Some(captures) = re.captures(offsetted_input) {
                        matched_captures = Some(captures);
                    }

                    // When `store_non_parsable_chars` is enabled, attempt to look forward to find
                    // a match if the match didn't work
                    if matched_captures.is_none() && store_non_parsable_chars && should_look_for_non_parsable_chars {
                        let mut matched_at_index = None;
                        for index in offset..input.len() {
                            // ref: https://stackoverflow.com/a/39239614/4115328
                            if let Some(captures) = re.captures(&input[index..]) {
                                matched_captures = Some(captures);
                                matched_at_index = Some(index);
                                // println!("MATCHED AT INDEX: {}", index);
                                break;
                            }
                        }

                        if let Some(index) = matched_at_index {
                            // Matched the token, but had to skip some characters to get there
                            println!("{}UNPARSABLE CHARS: offset={} index={} `{}`", depth_spaces, offset, index, &input[offset..index]);

                            // If a non parsable character hasn't been found yet in the stream,
                            // then note this one down as the first occurance.
                            //
                            // This data is used by the caller to weight different branches in the
                            // parse tree and figure out which one to pick
                            if first_non_parsable_char_index.is_none() {
                                first_non_parsable_char_index = Some(offset);
                            }

                            let mut new_token = Box::new(Token {
                                id: Uuid::new_v4(),
                                template: TokenMatchTemplateMatcher::Skipped,
                                literal: Some(String::from(&input[offset..index])),
                                matches: HashMap::new(),
                                effects: vec![],
                                events: TokenEvents::new_empty(),
                                next_id: None,
                                previous_id: None,
                                parent_id: None,
                                children_ids: vec![],
                            });
                            // if let Some(on_enter) = new_token.events.on_enter {
                            //     on_enter(&mut new_token);
                            // }

                            child_ids.push(new_token.id);

                            // println!("RW: {:?} <- {:?}", new_token.id, last_token_id);
                            previous_id_mapping.insert(new_token.id, last_token_id);
                            if let Some(last_token_id_unwrapped) = last_token_id {
                                // println!("RW: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                                next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                            }
                            last_token_id = Some(new_token.id);

                            tokens.push(new_token);

                            // If characters had to be skipped, it's definitely a partial match
                            matched_partial = true;

                            // Update the offset in the token stream to the place where the token
                            // was found
                            offset = index;
                            // matched_captures is set earlier above
                        } else {
                            // Did not match the token!
                            break;
                        }
                    }

                    let Some(captures) = matched_captures else {
                        break;
                    };


                    let Some(whole_match) = captures.get(0) else {
                        break;
                    };
                    let literal = whole_match.as_str();

                    // The optional negated_re parameter allows a secondary regex to be run
                    // on the match of the first regex, and if it matches, it fails the
                    // match
                    if let Some(negated_re_unwrapped) = negated_re {
                        if let Some(_) = negated_re_unwrapped.captures(literal) {
                            break;
                        }
                    }

                    let mut matches = HashMap::new();
                    for name in re.capture_names() {
                        let Some(name) = name else {
                            continue;
                        };

                        let Some(value) = captures.name(name) else {
                            continue;
                        };

                        matches.insert(name.to_string(), TokenMatch {
                            start: value.start(),
                            end: value.end(),
                            global_start: offset + value.start(),
                            global_end: offset + value.end(),
                            string: value.as_str().to_string(),
                        });
                    }

                    let new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: Some(String::from(literal)),
                        matches: matches,
                        effects: vec![],
                        events: match events {
                            Some(events) => events.clone(),
                            None => TokenEvents::new_empty(),
                        },
                        next_id: None,
                        previous_id: None,
                        parent_id: None,
                        children_ids: vec![],
                    });
                    child_ids.push(new_token.id);

                    // println!("RX: {:?} <- {:?}", new_token.id, last_token_id);
                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        // println!("RX: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }
                    last_token_id = Some(new_token.id);

                    tokens.push(new_token);

                    offset += whole_match.len();
                }
                TokenMatchTemplateMatcher::Any(matchers, events) => {
                    println!("{}ANY: {:?} {} {}", depth_spaces, matchers, escaped_offsetted_input, offset);
                    let mut full_match: Option<(
                        Box<Token>,
                        (TokenParseStatus, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensCollection),
                    )> = None;
                    let mut largest_partial_match: (usize, Option<usize>, Option<(
                        Box<Token>,
                        (TokenParseStatus, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensCollection),
                    )>) = (0, None, None);

                    // Attempt to find a pattern that fully matches
                    // And if one cannot be found, then find the largest partial match and go with
                    // that
                    for matcher in matchers {
                        let ephemeral_template = TokenMatchTemplate::new(
                            vec![matcher.clone()],
                        );

                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            effects: vec![],
                            events: match events {
                                Some(events) => events.clone(),
                                None => TokenEvents::new_empty(),
                            },
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });
                        if let Some(on_enter) = new_token.events.on_enter {
                            on_enter(&mut new_token);
                        }

                        let Ok((
                            ephemeral_parse_status,
                            ephemeral_offset,
                            ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens,
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            offset,
                            Some(new_token.id),
                            store_non_parsable_chars,
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            continue;
                        };

                        // If a full match was found, then we're done
                        if ephemeral_parse_status == TokenParseStatus::FullParse && ephemeral_offset != offset {
                        // if ephemeral_parse_status == TokenParseStatus::FullParse {
                            full_match = Some((new_token, (
                                ephemeral_parse_status,
                                ephemeral_offset,
                                ephemeral_last_token_id,
                                ephemeral_child_ids,
                                ephemeral_tokens,
                            )));
                            break;
                        }

                        // If a partial match was found, then store it for later
                        // If there are no full matches, this might be the best option we
                        // could do
                        if let TokenParseStatus::PartialParse(
                            number_of_chars_matched,
                            first_non_parsable_char_index,
                        ) = ephemeral_parse_status {
                            let pick_new = if let Some(existing_non_parsable_char_start_index) = largest_partial_match.1 {
                                // The existing match DOES have non parsable characters
                                // Does the current match?
                                if let Some(first_non_parsable_char_index) = first_non_parsable_char_index {
                                    // println!("BOTH HAVE NON PARSABLE");
                                    // Both matches do have non parsable characters, so pick the
                                    // match where the non parsable characters begin later in the
                                    // token stream
                                    let new_non_parsables_start_later = (
                                        existing_non_parsable_char_start_index < first_non_parsable_char_index
                                    );
                                    new_non_parsables_start_later
                                } else {
                                    // println!("OLD HAS NON PARSABLE");
                                    // The new match is a better match since it has no non
                                    // parsable chars
                                    true
                                }
                            } else {
                                // The existing match DOES NOT have non parsable characters
                                // Does the current match?
                                if let Some(first_non_parsable_char_index) = first_non_parsable_char_index {
                                    // println!("NEW HAS NON PARSABLE");
                                    // The existing match is a better match since it has no non
                                    // parsable chars

                                    // If there is no existing match yet though, this one is the
                                    // best we've got
                                    let no_existing_match = largest_partial_match.2.is_none();
                                    no_existing_match
                                } else {
                                    // println!("BOTH FULLY PARSABLE");
                                    // Both matches do NOT have non parsable characters
                                    // So the longest match should win!
                                    let is_largest_match = number_of_chars_matched > largest_partial_match.0;
                                    is_largest_match
                                }
                            };

                            if pick_new {
                                largest_partial_match = (
                                    number_of_chars_matched,
                                    first_non_parsable_char_index,
                                    Some((new_token, (
                                        ephemeral_parse_status,
                                        ephemeral_offset,
                                        ephemeral_last_token_id,
                                        ephemeral_child_ids,
                                        ephemeral_tokens,
                                    ))),
                                );
                            }
                        };
                    };

                    let fully_matched = match full_match {
                        Some(_) => true,
                        _ => false,
                    };
                    println!("{}`-- (fully_matched={} largest_partial_match.0={} largest_partial_match.1={:?})", depth_spaces, fully_matched, largest_partial_match.0, largest_partial_match.1);
                    let result = if let Some(result) = full_match {
                        Some(result)
                    } else if let (_, first_non_parsable_char_index_temp, Some(result)) = largest_partial_match {
                        if let Some(value) = first_non_parsable_char_index_temp {
                            first_non_parsable_char_index = Some(value);
                        }
                        Some(result)
                    } else {
                        None
                    };
                    let Some((mut new_token, (
                        ephemeral_parse_status,
                        ephemeral_offset,
                        ephemeral_last_token_id,
                        ephemeral_child_ids,
                        ephemeral_tokens,
                    ))) = result else {
                        // No match was found
                        break;
                    };

                    // If this reference matched partially, then this next match also is partial
                    if let TokenParseStatus::PartialParse(_, first_non_parsable_char_index_temp) = ephemeral_parse_status {
                        if let Some(value) = first_non_parsable_char_index_temp {
                            first_non_parsable_char_index = Some(value);
                        }
                        matched_partial = true;
                    };

                    // Link the new token's next_id to the first child
                    if let Some(first_ephemeral_child_id) = ephemeral_child_ids.first() {
                        // println!("AY: {:?} -> {:?}", new_token.id, Some(*first_ephemeral_child_id));
                        next_id_mapping.insert(new_token.id, Some(*first_ephemeral_child_id));
                    }

                    // Link the new token with its previous token
                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        // println!("AY: {:?} <- {:?}", last_token_id_unwrapped, Some(new_token.id));
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }

                    // Find the deepest last child in the ephemeral token hierarchy
                    let mut deep_last_ephemeral_child_id = ephemeral_child_ids.last();
                    loop {
                        let Some(deep_last_ephemeral_child_id_unwrapped) = deep_last_ephemeral_child_id else {
                            break;
                        };
                        let Some(child_token) = ephemeral_tokens.get_by_id(
                            *deep_last_ephemeral_child_id_unwrapped
                        ) else {
                            break;
                        };
                        if let Some(result) = child_token.children_ids.last() {
                            deep_last_ephemeral_child_id = Some(result);
                        } else {
                            break;
                        }
                    }
                    last_token_id = if let Some(deep_last_ephemeral_child_id) = deep_last_ephemeral_child_id {
                        // The next "last token" should be the last child
                        Some(*deep_last_ephemeral_child_id)
                    } else {
                        // The next "last token" should be the token that was just added
                        Some(new_token.id)
                    };

                    new_token.children_ids = ephemeral_child_ids;
                    child_ids.push(new_token.id);

                    let new_token_id = new_token.id;
                    tokens.push(new_token);

                    let mut temp_ephemeral_tokens_without_parents: Vec<Box<Token>> = vec![];
                    // Add all tokens to the tokens array that don't have parents
                    for token in ephemeral_tokens.tokens {
                        if token.parent_id == None {
                            temp_ephemeral_tokens_without_parents.push(token);
                            continue
                        };
                        tokens.push(token);
                    }
                    // Then add all the tokens with parents AFTER so that when `on_leave` is
                    // called, all the child tokens (that are already parented) will be present
                    // in `tokens` first
                    for mut token in temp_ephemeral_tokens_without_parents {
                        token.parent_id = Some(new_token_id);
                        if let Some(on_leave) = token.events.on_leave {
                            on_leave(&mut token, &mut tokens);
                        }
                        tokens.push(token);
                    }

                    offset = ephemeral_offset;
                }
                TokenMatchTemplateMatcher::Sequence(matchers, events) => {
                    println!("{}REPEAT({:?}): {} {}", depth_spaces, template_matcher, escaped_offsetted_input, offset);
                    let new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut new_last_token_id = last_token_id;

                    // An empty sequence should be treated as a no-op
                    if matchers.len() == 0 {
                        continue;
                    }

                    // Attempt to match the ephemeral template at least `min_repeat` times:
                    let mut match_failed = false;
                    for matcher in matchers {
                        let ephemeral_template = TokenMatchTemplate::new(
                            vec![matcher.clone()],
                        );

                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            effects: vec![],
                            events: match events {
                                Some(events) => events.clone(),
                                None => TokenEvents::new_empty(),
                            },
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });
                        if let Some(on_enter) = new_token.events.on_enter {
                            on_enter(&mut new_token);
                        }

                        let Ok((
                            ephemeral_parse_status,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            store_non_parsable_chars,
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            match_failed = true;
                            break;
                        };

                        // only actually store new tokens if a match was found
                        if ephemeral_parse_status == TokenParseStatus::Failed {
                            match_failed = true;
                            break;
                        }

                        // Another match was found, so store another generated token
                        new_offset = ephemeral_offset;

                        // Link the new token's next_id to the first child
                        if let Some(first_ephemeral_child_id) = ephemeral_child_ids.first() {
                            // println!("RC: {:?} -> {:?}", new_token.id, Some(*first_ephemeral_child_id));
                            next_id_mapping.insert(new_token.id, Some(*first_ephemeral_child_id));
                        }

                        // println!("RC: {:?} <- {:?}", new_token.id, new_last_token_id);
                        previous_id_mapping.insert(new_token.id, new_last_token_id);
                        if let Some(last_token_id_unwrapped) = new_last_token_id {
                            // println!("RC: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }

                        // Find the deepest last child in the ephemeral token hierarchy
                        let mut deep_last_ephemeral_child_id = ephemeral_child_ids.last();
                        loop {
                            let Some(deep_last_ephemeral_child_id_unwrapped) = deep_last_ephemeral_child_id else {
                                break;
                            };
                            let Some(child_token) = ephemeral_tokens.get_by_id(
                                *deep_last_ephemeral_child_id_unwrapped
                            ) else {
                                break;
                            };
                            let Some(result) = child_token.children_ids.last() else {
                                break;
                            };
                            deep_last_ephemeral_child_id = Some(result);
                        }
                        new_last_token_id = if let Some(deep_last_ephemeral_child_id) = deep_last_ephemeral_child_id {
                            // The next "last token" should be the last child
                            Some(*deep_last_ephemeral_child_id)
                        } else {
                            // The next "last token" should be the token that was just added
                            Some(new_token.id)
                        };
                        // println!("RC: new_last_token_id={:?} foo={:?}", new_last_token_id, ephemeral_child_ids);

                        new_token.children_ids = ephemeral_child_ids;
                        child_ids.push(new_token.id);

                        let new_token_id = new_token.id;
                        tokens.push(new_token);

                        let mut temp_ephemeral_tokens_without_parents: Vec<Box<Token>> = vec![];
                        // Add all tokens to the tokens array that don't have parents
                        for token in ephemeral_tokens.tokens {
                            if token.parent_id == None {
                                temp_ephemeral_tokens_without_parents.push(token);
                                continue
                            };
                            tokens.push(token);
                        }
                        // Then add all the tokens with parents AFTER so that when `on_leave` is
                        // called, all the child tokens (that are already parented) will be present
                        // in `tokens` first
                        for mut token in temp_ephemeral_tokens_without_parents {
                            token.parent_id = Some(new_token_id);
                            if let Some(on_leave) = token.events.on_leave {
                                on_leave(&mut token, &mut tokens);
                            }
                            tokens.push(token);
                        }

                        // If this reference matched partially, then we're done since this is where
                        // the parser looses track of where it's going
                        if let TokenParseStatus::PartialParse(_, first_non_parsable_char_index_temp) = ephemeral_parse_status {
                            if let Some(value) = first_non_parsable_char_index_temp {
                                first_non_parsable_char_index = Some(value);
                            }
                            matched_partial = true;
                            break;
                        };
                    }
                    println!("{}`-- match_failed={}", depth_spaces, match_failed);

                    if match_failed {
                        break;
                    }

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                    last_token_id = new_last_token_id;
                }
                TokenMatchTemplateMatcher::RepeatCount(boxed_matcher, min_repeats, max_repeats, events) => {
                    println!("{}REPEAT({:?}): {} {}", depth_spaces, template_matcher, escaped_offsetted_input, offset);
                    let new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut new_last_token_id = last_token_id;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate::new(
                        vec![*boxed_matcher.clone()],
                    );

                    // Attempt to match the ephemeral template at least `min_repeat` times:
                    let mut matched_at_least_once = false;
                    for _index in 0..*max_repeats {
                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            effects: vec![],
                            events: match events {
                                Some(events) => events.clone(),
                                None => TokenEvents::new_empty(),
                            },
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });
                        if let Some(on_enter) = new_token.events.on_enter {
                            on_enter(&mut new_token);
                        }

                        let Ok((
                            ephemeral_parse_status,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            store_non_parsable_chars,
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            break;
                        };
                        // only actually store a new token if a match was found
                        if ephemeral_parse_status == TokenParseStatus::Failed {
                            break;
                        }
                        matched_at_least_once = true;

                        // Another match was found, so store another generated token
                        repeat_count += 1;
                        new_offset = ephemeral_offset;



                        // Link the new token's next_id to the first child
                        if let Some(first_ephemeral_child_id) = ephemeral_child_ids.first() {
                            // println!("RC: {:?} -> {:?}", new_token.id, Some(*first_ephemeral_child_id));
                            next_id_mapping.insert(new_token.id, Some(*first_ephemeral_child_id));
                        }

                        // println!("RC: {:?} <- {:?}", new_token.id, new_last_token_id);
                        previous_id_mapping.insert(new_token.id, new_last_token_id);
                        if let Some(last_token_id_unwrapped) = new_last_token_id {
                            // println!("RC: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }

                        // Find the deepest last child in the ephemeral token hierarchy
                        let mut deep_last_ephemeral_child_id = ephemeral_child_ids.last();
                        loop {
                            let Some(deep_last_ephemeral_child_id_unwrapped) = deep_last_ephemeral_child_id else {
                                break;
                            };
                            let Some(child_token) = ephemeral_tokens.get_by_id(
                                *deep_last_ephemeral_child_id_unwrapped
                            ) else {
                                break;
                            };
                            let Some(result) = child_token.children_ids.last() else {
                                break;
                            };
                            deep_last_ephemeral_child_id = Some(result);
                        }
                        new_last_token_id = if let Some(deep_last_ephemeral_child_id) = deep_last_ephemeral_child_id {
                            // The next "last token" should be the last child
                            Some(*deep_last_ephemeral_child_id)
                        } else {
                            // The next "last token" should be the token that was just added
                            Some(new_token.id)
                        };
                        // println!("RC: new_last_token_id={:?} foo={:?}", new_last_token_id, ephemeral_child_ids);

                        new_token.children_ids = ephemeral_child_ids;
                        child_ids.push(new_token.id);

                        let new_token_id = new_token.id;
                        tokens.push(new_token);

                        let mut temp_ephemeral_tokens_without_parents: Vec<Box<Token>> = vec![];
                        // Add all tokens to the tokens array that don't have parents
                        for token in ephemeral_tokens.tokens {
                            if token.parent_id == None {
                                temp_ephemeral_tokens_without_parents.push(token);
                                continue
                            };
                            tokens.push(token);
                        }
                        // Then add all the tokens with parents AFTER so that when `on_leave` is
                        // called, all the child tokens (that are already parented) will be present
                        // in `tokens` first
                        for mut token in temp_ephemeral_tokens_without_parents {
                            token.parent_id = Some(new_token_id);
                            if let Some(on_leave) = token.events.on_leave {
                                on_leave(&mut token, &mut tokens);
                            }
                            tokens.push(token);
                        };

                        // If this reference matched partially, then we're done since this is where
                        // the parser looses track of where it's going
                        if let TokenParseStatus::PartialParse(_, first_non_parsable_char_index_temp) = ephemeral_parse_status {
                            if let Some(value) = first_non_parsable_char_index_temp {
                                first_non_parsable_char_index = Some(value);
                            }
                            matched_partial = true;
                            break;
                        };
                    }
                    println!("{}`-- (matched_at_least_once={} repeat_count={})", depth_spaces, matched_at_least_once, repeat_count);

                    // NOTE: if the repeat count can be zero and the match fails, that is totally
                    // fine
                    if *min_repeats != 0 && !matched_at_least_once {
                        break;
                    }

                    if repeat_count < *min_repeats {
                        break;
                    }
                    if repeat_count > *max_repeats {
                        break;
                    }

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                    last_token_id = new_last_token_id;
                }
                TokenMatchTemplateMatcher::RepeatOnceToForever(boxed_matcher, events) |
                TokenMatchTemplateMatcher::RepeatZeroToForever(boxed_matcher, events) => {
                    let min_repeats = match template_matcher {
                        TokenMatchTemplateMatcher::RepeatOnceToForever(_, _) => 1,
                        TokenMatchTemplateMatcher::RepeatZeroToForever(_, _) => 0,
                        _ => 0, // NOTE: this should never be hit
                    };
                    println!("{}FOREVER: {} {} {}", depth_spaces, escaped_offsetted_input, offset, min_repeats);
                    let new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut new_last_token_id = last_token_id;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate::new(
                        vec![*boxed_matcher.clone()],
                    );

                    loop {
                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            effects: vec![],
                            events: match events {
                                Some(events) => events.clone(),
                                None => TokenEvents::new_empty(),
                            },
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });
                        if let Some(on_enter) = new_token.events.on_enter {
                            on_enter(&mut new_token);
                        }

                        let Ok((
                            ephemeral_parse_status,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens,
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            store_non_parsable_chars,
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            break;
                        };

                        // only actually store a new token if a match was found
                        if ephemeral_parse_status == TokenParseStatus::Failed {
                            break;
                        }

                        // Another match was found, so store another generated token
                        repeat_count += 1;
                        new_offset = ephemeral_offset;

                        // Link the new token's next_id to the first child
                        if let Some(first_ephemeral_child_id) = ephemeral_child_ids.first() {
                            // println!("F: {:?} -> {:?}", new_token.id, Some(*first_ephemeral_child_id));
                            next_id_mapping.insert(new_token.id, Some(*first_ephemeral_child_id));
                        }

                        // println!("F: {:?} <- {:?}", new_token.id, new_last_token_id);
                        previous_id_mapping.insert(new_token.id, new_last_token_id);
                        if let Some(last_token_id_unwrapped) = new_last_token_id {
                            // println!("F: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }

                        // Find the deepest last child in the ephemeral token hierarchy
                        let mut deep_last_ephemeral_child_id = ephemeral_child_ids.last();
                        loop {
                            let Some(deep_last_ephemeral_child_id_unwrapped) = deep_last_ephemeral_child_id else {
                                break;
                            };
                            let Some(child_token) = ephemeral_tokens.get_by_id(
                                *deep_last_ephemeral_child_id_unwrapped
                            ) else {
                                break;
                            };
                            let Some(result) = child_token.children_ids.last() else {
                                break;
                            };
                            deep_last_ephemeral_child_id = Some(result);
                        }
                        // println!("        {:?}", deep_last_ephemeral_child_id);
                        new_last_token_id = if let Some(deep_last_ephemeral_child_id) = deep_last_ephemeral_child_id {
                            // The next "last token" should be the last child
                            Some(*deep_last_ephemeral_child_id)
                        } else {
                            // The next "last token" should be the token that was just added
                            Some(new_token.id)
                        };
                        // println!("F: new_last_token_id={:?} foo={:?}", new_last_token_id, ephemeral_child_ids);

                        new_token.children_ids = ephemeral_child_ids;
                        child_ids.push(new_token.id);

                        let new_token_id = new_token.id;
                        tokens.push(new_token);

                        let mut temp_ephemeral_tokens_without_parents: Vec<Box<Token>> = vec![];
                        // Add all tokens to the tokens array that don't have parents
                        for token in ephemeral_tokens.tokens {
                            if token.parent_id == None {
                                temp_ephemeral_tokens_without_parents.push(token);
                                continue
                            };
                            tokens.push(token);
                        }
                        // Then add all the tokens with parents AFTER so that when `on_leave` is
                        // called, all the child tokens (that are already parented) will be present
                        // in `tokens` first
                        for mut token in temp_ephemeral_tokens_without_parents {
                            token.parent_id = Some(new_token_id);
                            if let Some(on_leave) = token.events.on_leave {
                                on_leave(&mut token, &mut tokens);
                            }
                            tokens.push(token);
                        };

                        // If this reference matched partially, then we're done since this is where
                        // the parser looses track of where it's going
                        if let TokenParseStatus::PartialParse(_, first_non_parsable_char_index_temp) = ephemeral_parse_status {
                            if let Some(value) = first_non_parsable_char_index_temp {
                                first_non_parsable_char_index = Some(value);
                            }
                            matched_partial = true;
                            break;
                        };
                    }
                    println!("{}`-- (offset={} repeat_count={})", depth_spaces, offset, repeat_count);

                    if repeat_count < min_repeats {
                        break;
                    }

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                    last_token_id = new_last_token_id;
                }
                // NOTE: this pattern should never appear in a user generated token template map
                //
                // It's used for skipping characters in the input stream when this is desired to
                // force match a given pattern
                TokenMatchTemplateMatcher::Skipped => {}
            }
            matched_token_count += 1;
        }

        // When `store_non_parsable_chars` is passed, at the very end of the computation, take all
        // the remaining characters at the end and add them as TokenMatchTemplateMatcher::Skipped
        // entries
        if depth == 0 && store_non_parsable_chars {
            let remaining_chars = &input[offset..];
            if remaining_chars.len() > 0 {
                // Matched the token, but had to skip some characters to get there
                println!("{}FINAL UNPARSABLE CHARS: `{}`", depth_spaces, remaining_chars);

                // If a non parsable character hasn't been found yet in the stream,
                // then note this one down as the first occurance.
                //
                // This data is used by the caller to weight different branches in the
                // parse tree and figure out which one to pick
                if first_non_parsable_char_index.is_none() {
                    first_non_parsable_char_index = Some(offset);
                }

                let mut new_token = Box::new(Token {
                    id: Uuid::new_v4(),
                    template: TokenMatchTemplateMatcher::Skipped,
                    literal: Some(String::from(remaining_chars)),
                    matches: HashMap::new(),
                    effects: vec![],
                    events: TokenEvents::new_empty(),
                    next_id: None,
                    previous_id: None,
                    parent_id: None,
                    children_ids: vec![],
                });
                // if let Some(on_enter) = new_token.events.on_enter {
                //     on_enter(&mut new_token);
                // }

                child_ids.push(new_token.id);

                // println!("RW: {:?} <- {:?}", new_token.id, last_token_id);
                previous_id_mapping.insert(new_token.id, last_token_id);
                if let Some(last_token_id_unwrapped) = last_token_id {
                    // println!("RW: {:?} -> {:?}", last_token_id_unwrapped, Some(new_token.id));
                    next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                }
                last_token_id = Some(new_token.id);

                tokens.push(new_token);

                // If characters had to be skipped, it's definitely now a partial match
                matched_partial = true;

                // Update the offset in the token stream to the place where the token
                // was found
                offset += remaining_chars.len();
            }
        }

        let parented_tokens: Vec<Box<Token>> = tokens.tokens.into_iter().map(|mut token| {
            if let Some(next_id) = next_id_mapping.get(&token.id) {
                // println!("  NEXT SET: {} {:?}", token.id, next_id);
                token.next_id = *next_id;
            };
            if let Some(previous_id) = previous_id_mapping.get(&token.id) {
                token.previous_id = *previous_id;
            };

            token
        }).collect();

        // for mut token in &mut parented_tokens {
        //     if token.parent_id == None {
        //         continue;
        //     }
        //     if let Some(on_leave) = token.events.on_leave {
        //         on_leave(&mut token, &mut parented_tokens);
        //     }
        // }


        let matched_all_tokens = matched_token_count == self.matcher.len();
        println!("{}`-- matched_token_count={} self.matcher.len()={}", depth_spaces, matched_token_count, self.matcher.len());
        let status = if matched_partial {
            TokenParseStatus::PartialParse(offset - initial_offset, first_non_parsable_char_index)
        } else if matched_all_tokens {
            TokenParseStatus::FullParse
        } else {
            TokenParseStatus::Failed
        };
        println!("{}`-- matched_all_tokens={} matched_partial={} status={:?}", depth_spaces, matched_all_tokens, matched_partial, status);
        Ok((status, offset, last_token_id, child_ids, TokensCollection::new(parented_tokens)))
    }
}
