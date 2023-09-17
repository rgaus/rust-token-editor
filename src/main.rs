use std::collections::HashMap;
use regex::Regex;
use uuid::Uuid;

mod token;
use token::*;

#[derive(Debug)]
#[derive(Clone)]
struct TokenMatchTemplate {
    matcher: Vec<TokenMatchTemplateMatcher>,
    events: Option<TokenEvents>,
}

const TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH: usize = 99;

impl TokenMatchTemplate {
    fn new(matcher: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher, events: None }
    }
    fn new_with_events(matcher: Vec<TokenMatchTemplateMatcher>, events: TokenEvents) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher, events: Some(events) }
    }
    fn consume_from_start(
        &self,
        input: &str,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(bool, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensBag), String> {
        self.consume_from_offset(input, 0, None, 0, token_match_templates_map)
    }
    fn consume_from_offset(
        &self,
        input: &str,
        initial_offset: usize,
        initial_last_token_id: Option<uuid::Uuid>,
        depth: usize,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(bool, usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, TokensBag), String> {
        let mut tokens: Vec<Box<Token>> = vec![];
        let mut offset = initial_offset;
        let mut child_ids: Vec<uuid::Uuid> = vec![];

        if depth > TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH {
            return Ok((false, offset, initial_last_token_id, child_ids, tokens))
        };

        let mut depth_spaces = String::from("");
        for _ in 0..depth {
            // depth_spaces = format!("{}| ", depth_spaces);
            depth_spaces = format!("{}  ", depth_spaces);
        }

        let mut last_token_id: Option<uuid::Uuid> = initial_last_token_id;
        let mut next_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();
        let mut previous_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();

        let mut matched_token_count = 0;
        for template_matcher in &self.matcher {
            if offset > input.len()-1 {
                println!("{}EOF!", depth_spaces);
                break;
            };

            let offsetted_input = &input[offset..];
            let escaped_offsetted_input = format!("`{}`", offsetted_input.replace("\n", "\\n"));
            match template_matcher {
                TokenMatchTemplateMatcher::Raw(raw, events) => {
                    println!("{}RAW({}): {} {}", depth_spaces, raw, escaped_offsetted_input, offset);
                    if !offsetted_input.starts_with(raw) {
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
                        referenced_matched_all_tokens,
                        referenced_template_offset,
                        _referenced_last_token_id,
                        referenced_child_ids,
                        referenced_template_tokens,
                    )) = referenced_template.consume_from_offset(
                        input,
                        offset,
                        last_token_id,
                        depth + 1,
                        token_match_templates_map,
                    ) else {
                        break;
                    };
                    println!("{}`-- (referenced_matched_all_tokens={})", depth_spaces, referenced_matched_all_tokens);
                    if !referenced_matched_all_tokens {
                        break;
                    }

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
                        let Some(child_token) = referenced_template_tokens.iter().find(
                            |t| t.id == *deep_last_referenced_child_id_unwrapped
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
                    for token in referenced_template_tokens {
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
                    println!("{}REGEX({:?}): {} {}", depth_spaces, re, escaped_offsetted_input, offset);
                    // ref: https://stackoverflow.com/a/39239614/4115328
                    match re.captures(offsetted_input) {
                        Some(captures) => {
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
                        None => {
                            break;
                        }
                    }
                }
                TokenMatchTemplateMatcher::Any(matchers, events) => {
                    println!("{}ANY: {:?} {} {}", depth_spaces, matchers, escaped_offsetted_input, offset);
                    let mut matched_at_least_one = false;
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
                            ephemeral_matched_all_tokens,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            continue;
                        };
                        // only actually store a new token if a match was found
                        if !ephemeral_matched_all_tokens {
                            continue;
                        }

                        // Only actually store a new token if a match was found
                        if ephemeral_offset == offset {
                            continue;
                        }

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
                            let Some(child_token) = ephemeral_tokens.iter().find(
                                |t| t.id == *deep_last_ephemeral_child_id_unwrapped
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
                        for token in ephemeral_tokens {
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
                        matched_at_least_one = true;
                        break;
                    }
                    println!("{}`-- (matched_at_least_one={})", depth_spaces, matched_at_least_one);
                    if !matched_at_least_one {
                        break;
                    }
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
                            ephemeral_matched_all_tokens,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            match_failed = true;
                            break;
                        };
                        // only actually store new tokens if a match was found
                        if !ephemeral_matched_all_tokens {
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
                            let Some(child_token) = ephemeral_tokens.iter().find(
                                |t| t.id == *deep_last_ephemeral_child_id_unwrapped
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
                        for token in ephemeral_tokens {
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
                        }
                    }
                    println!("{}`-- (match_failed={}", depth_spaces, match_failed);

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
                    let mut match_failed = false;
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
                            ephemeral_matched_all_tokens,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            match_failed = true;
                            break;
                        };
                        // only actually store a new token if a match was found
                        if !ephemeral_matched_all_tokens {
                            match_failed = true;
                            break;
                        }

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
                            let Some(child_token) = ephemeral_tokens.iter().find(
                                |t| t.id == *deep_last_ephemeral_child_id_unwrapped
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
                        for token in ephemeral_tokens {
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
                    }
                    println!("{}`-- (match_failed={} repeat_count={})", depth_spaces, match_failed, repeat_count);

                    // NOTE: if the repeat count can be zero and the match fails, that is totally
                    // fine
                    if *min_repeats != 0 && match_failed {
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
                            ephemeral_matched_all_tokens,
                            ephemeral_offset,
                            _ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens,
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            break;
                        };

                        // only actually store a new token if a match was found
                        if !ephemeral_matched_all_tokens {
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
                            let Some(child_token) = ephemeral_tokens.iter().find(
                                |t| t.id == *deep_last_ephemeral_child_id_unwrapped
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
                        for token in ephemeral_tokens {
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
            }
            matched_token_count += 1;
        }

        let parented_tokens: Vec<Box<Token>> = tokens.into_iter().map(|mut token| {
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
        println!("{}`-- matched_all_tokens={}", depth_spaces, matched_all_tokens);
        Ok((matched_all_tokens, offset, last_token_id, child_ids, parented_tokens))
    }
}

fn dump_inner(tokens: &Vec<Box<Token>>, child_ids: Vec<uuid::Uuid>, indent: String) {
    for child_id in child_ids {
        let Some(token) = tokens.iter().find(|t| t.id == child_id) else {
            continue;
        };
        // println!("{}{:?} id:{} next:{:?} prev:{:?} \t_{}_\t==> {:?}", indent, token.template, token.id, token.next_id, token.previous_id, match &token.literal {
        //     Some(n) => n,
        //     None => "",
        // }, token.matches);
        // println!("{}{:?} \t_{}_\t==> {:?} {:?}", indent, token.template, match &token.literal {
        //     Some(n) => n,
        //     None => "",
        // }, token.matches, token.effects);
        println!("{}{:?} => {:?}", indent, token.template, token.effects);
        dump_inner(tokens, token.children_ids.clone(), format!("{}  ", indent));
    }
}
fn dump(head_id: uuid::Uuid, tokens: &Vec<Box<Token>>) {
    dump_inner(tokens, vec![head_id], "".to_string());
}

fn stringify(
    head_id: uuid::Uuid,
    tokens: &mut Vec<Box<Token>>,
    token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
) -> String {
    let mut result = String::from("");
    let mut pointer_id = head_id;
    loop {
        let Some(mut pointer) = tokens.iter().find(|t| t.id == pointer_id) else {
            continue;
        };
        if let Some(literal_text) = &pointer.literal {
            if literal_text == "456" {
                let (Some(mut_pointer), mut tokens) = get_mut_token_in_tokensbag(tokens, pointer_id) else {
                    continue;
                };
                change_token_literal_text(
                    mut_pointer,
                    result.len(),
                    "a a".to_string(),
                    &mut tokens,
                    token_match_templates_map,
                );
                break;
            }
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


fn change_token_literal_text<'a>(
    token: &'a mut Box<Token>,
    token_offset: usize,
    new_text: String,
    tokens: &'a mut TokensBag,
    token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
) {
    token.literal = Some(new_text.clone());

    let mut depth = token.depth(tokens);
    
    let mut working_token: Box<Token> = token.clone();
    let mut working_template = TokenMatchTemplate::new(
        vec![token.template.clone()],
    );

    loop {
        println!("offset {}", token_offset);
        match working_template.consume_from_offset(
            &new_text,
            token_offset,
            working_token.previous_id,
            depth,
            token_match_templates_map,
        ) {
            Ok((matched_all, offset, _last_token_id, child_ids, mut new_tokens)) => {
                println!("MATCHED ALL? {} {}", offset, matched_all);
                if matched_all {
                    println!("MATCHED ALL!");
                    break;
                }

                let Some(parent) = working_token.parent(&tokens) else {
                    println!("NO PARENT!");
                    break;
                };

                working_template = TokenMatchTemplate::new(
                    vec![parent.template.clone()],
                );
                working_token = parent.clone();
            }
            Err(e) => {
                println!("ERROR: {:?}", e);
                break;
            }
        }
    }
    // If the match fails, then go up a level to the parent token
    // And then try to match again
    //
    // NOTE: another thing to do here could be to increase the index by one and attempt
    // to match it again - this would disregard bogus characters and potentially allow
    // a string with a trantient error to reparse efficiently
}






fn main() {
    let mut token_match_templates_map = HashMap::new();
    token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::repeat_zero_to_forever(Box::new(
            TokenMatchTemplateMatcher::any(vec![
                TokenMatchTemplateMatcher::reference("Whitespace"),
                TokenMatchTemplateMatcher::reference("Statement"),
                TokenMatchTemplateMatcher::reference("Block"),
            ]),
        )),
    ]));

    token_match_templates_map.insert("Block", TokenMatchTemplate::new_with_events(vec![
        TokenMatchTemplateMatcher::raw("{"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::repeat_once_to_forever(Box::new(
            TokenMatchTemplateMatcher::reference("StatementWithWhitespace")
        )),
        TokenMatchTemplateMatcher::raw("}"),
    ], TokenEvents {
        on_enter: Some(|token| {
            token.effects.push(TokenEffect::DeclareLexicalScope);
        }),
        on_leave: None,
        // on_leave: Some(|token, tokens| {
        //     let Some(parent_id) = token.parent_id else {
        //         return;
        //     };
        //     println!("DETAILS ID: {:?}", parent_id);
        //     let Some(parent_token) = tokens.iter().find(|t| t.id == parent_id) else {
        //         return;
        //     };
        //     println!("DETAILS: {:?}", parent_token);
        // }),
    }));

    token_match_templates_map.insert("StatementWithWhitespace", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::any(vec![
            TokenMatchTemplateMatcher::reference("Statement"),
            TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        ]),
    ]));

    token_match_templates_map.insert("Statement", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::any(vec![
            TokenMatchTemplateMatcher::reference("Declaration"),
            TokenMatchTemplateMatcher::reference("Expression"),
        ]),
    ]));

    token_match_templates_map.insert("Declaration", TokenMatchTemplate::new_with_events(vec![
        TokenMatchTemplateMatcher::raw("let"),
        TokenMatchTemplateMatcher::reference("Whitespace"),
        TokenMatchTemplateMatcher::reference("Identifier"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::raw("="),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::reference("Expression"),
    ], TokenEvents {
        on_enter: None,
        on_leave: Some(|token, tokens| {
            let Some(TokenEffect::DeclareIdentifier(identifier)) = token.find_child_effect(tokens, |e| {
                if let TokenEffect::DeclareIdentifier(_) = e { true } else { false }
            }) else { return; };

            let Some(TokenEffect::DeclareExpression(expression)) = token.find_child_effect(tokens, |e| {
                if let TokenEffect::DeclareExpression(_) = e { true } else { false }
            }) else { return; };

            token.effects.push(TokenEffect::DeclareMember {
                name: identifier.clone(),
                value: expression.clone(),
            });
        }),
    }));

    token_match_templates_map.insert("StringLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::regex_with_events(
            Regex::new(r"^(?<literal>'[^']*')").unwrap(),
            TokenEvents {
                on_enter: None,
                on_leave: Some(|token, tokens| {
                    let Some(parent) = token.mut_parent(tokens) else {
                        return;
                    };
                    parent.effects.push(TokenEffect::DeclareExpression(
                        token.matches.get("literal").unwrap().string.clone(),
                    ));
                }),
            },
        ),
    ]));
    token_match_templates_map.insert("NumberLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::regex(
            Regex::new(r"^(?<literal>[0-9]+)").unwrap(),
        ),
    ]));
    token_match_templates_map.insert("HashLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::raw("{"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::repeat_zero_to_forever(Box::new(
            TokenMatchTemplateMatcher::sequence(vec![
                TokenMatchTemplateMatcher::reference("HashLiteralEntry"),
                TokenMatchTemplateMatcher::raw(","),
                TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
            ]),
        )),
        TokenMatchTemplateMatcher::repeat_count(Box::new(
            TokenMatchTemplateMatcher::reference("HashLiteralEntry"),
        ), 0, 1),
        TokenMatchTemplateMatcher::repeat_count(Box::new(
            TokenMatchTemplateMatcher::raw(","),
        ), 0, 1),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::raw("}"),
    ]));
    token_match_templates_map.insert("HashLiteralEntry", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::reference("Expression"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::raw(":"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::reference("Expression"),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
    ]));

    token_match_templates_map.insert("ArrayLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::raw("["),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::repeat_zero_to_forever(Box::new(
            TokenMatchTemplateMatcher::sequence(vec![
                TokenMatchTemplateMatcher::reference("ArrayLiteralEntry"),
                TokenMatchTemplateMatcher::raw(","),
                TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
            ]),
        )),
        TokenMatchTemplateMatcher::repeat_count(Box::new(
            TokenMatchTemplateMatcher::reference("ArrayLiteralEntry"),
        ), 0, 1),
        TokenMatchTemplateMatcher::repeat_count(Box::new(
            TokenMatchTemplateMatcher::raw(","),
        ), 0, 1),
        TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
        TokenMatchTemplateMatcher::raw("]"),
    ]));
    token_match_templates_map.insert("ArrayLiteralEntry", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::reference("Expression"),
    ]));

    token_match_templates_map.insert("Variable", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::reference("Identifier"),
    ]));

    token_match_templates_map.insert("Expression", TokenMatchTemplate::new_with_events(vec![
        TokenMatchTemplateMatcher::any(vec![
            TokenMatchTemplateMatcher::reference("StringLiteral"),
            TokenMatchTemplateMatcher::reference("NumberLiteral"),
            TokenMatchTemplateMatcher::reference("HashLiteral"),
            TokenMatchTemplateMatcher::reference("ArrayLiteral"),
            TokenMatchTemplateMatcher::reference("Variable"),
            TokenMatchTemplateMatcher::reference("Block"),
        ]),
    ], TokenEvents {
        on_enter: None,
        on_leave: Some(|token, tokens| {
            // let Some(next) = token.next(tokens) else {
            //     return;
            // };
            //
            // let expression = next.find_child_effect(tokens, |e| {
            //     if let TokenEffect::DeclareExpression(_) = e { true } else { false }
            // }).unwrap();
            //
            // token.effects.push(expression.clone());
        }),
    }));

    token_match_templates_map.insert("Identifier", TokenMatchTemplate::new_with_events(vec![
        // TokenMatchTemplateMatcher::regex_and_negation(
        //     Regex::new(r"^(?<value>[a-zA-Z](?:[a-zA-Z0-9_\$])*)").unwrap(),
        //     Regex::new(r"^(let)$").unwrap(),
        // ),

        TokenMatchTemplateMatcher::regex_and_negation_with_events(
            Regex::new(r"^(?<value>[a-zA-Z](?:[a-zA-Z0-9_\$])*)").unwrap(),
            Regex::new(r"^(let)$").unwrap(),
            TokenEvents {
                on_enter: None,
                on_leave: Some(|token, tokens| {
                    let Some(parent) = token.mut_parent(tokens) else {
                        return;
                    };
                    parent.effects.push(TokenEffect::DeclareIdentifier(
                        token.matches.get("value").unwrap().string.clone(),
                    ));
                }),
            },
        ),
    ], TokenEvents { on_enter: None, on_leave: None }));

    token_match_templates_map.insert("OptionalWhitespace", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::repeat_count(Box::new(
            TokenMatchTemplateMatcher::regex(
                Regex::new(r"^\s+").unwrap(),
            ),
        ), 0, 1),
    ]));

    token_match_templates_map.insert("Whitespace", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::regex(
            Regex::new(r"^\s+").unwrap(),
        ),
    ]));

    let Some(all_template) = token_match_templates_map.get("All") else {
        panic!("No 'All' template found!");
    };

//     let input = "
// let b = {
//     'foo': 2,
//     'nested': {
//         'again': [5, 6]
//     }
// }
// {
//     {
//         let a = 'aaa'
//     }
// }";

    // let input = "{let a = 'aaa'}";
    let input = "456";

    // let input = "1aa1bb";

    match all_template.consume_from_start(input, &token_match_templates_map) {
        Ok((_matched_all, offset, _last_token_id, child_ids, mut tokens)) => {
            // println!("RESULT: {:?} {:?}", offset, tokens);
            println!("Offset: {}\nInput:\n{}\n---\n", offset, input);

            println!("=========");
            println!("= TOKENS: {} {}", tokens.len(), input.len());
            println!("=========");

            for child_id in &child_ids {
                dump(*child_id, &tokens);
                println!("---------");
            }

            println!("=========");
            println!("= STRINGS:");
            println!("=========");

            if !child_ids.is_empty() {
                println!("{}", stringify(child_ids[0], &mut tokens, &token_match_templates_map));
            }
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}
