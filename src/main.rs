use std::collections::HashMap;
use regex::Regex;
use uuid::Uuid;

#[derive(Debug)]
struct TokenMatch {
    start: usize,
    end: usize,
    global_start: usize,
    global_end: usize,
    string: String,
}

#[derive(Debug)]
struct Token {
    id: uuid::Uuid,
    template: TokenMatchTemplateMatcher,
    literal: Option<String>,
    matches: HashMap<String, TokenMatch>,

    next_id: Option<uuid::Uuid>,
    previous_id: Option<uuid::Uuid>,
    parent_id: Option<uuid::Uuid>,
    children_ids: Vec<uuid::Uuid>,
}

#[derive(Debug)]
#[derive(Clone)]
enum TokenMatchTemplateMatcher {
    Raw(&'static str),
    Reference(&'static str),
    Regex(regex::Regex),
    Any(Vec<TokenMatchTemplateMatcher>),
    RepeatCount(Box<TokenMatchTemplateMatcher>, usize, usize),
    RepeatForever(Box<TokenMatchTemplateMatcher>),
}

#[derive(Debug)]
#[derive(Clone)]
struct TokenMatchTemplate {
    matcher: Vec<TokenMatchTemplateMatcher>
}

const TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH: usize = 99;

impl TokenMatchTemplate {
    fn new(matcher: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher }
    }
    fn consume_from_start(
        &self,
        input: &str,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, Vec<Box<Token>>), String> {
        self.consume_from_offset(input, 0, None, 0, token_match_templates_map)
    }
    fn consume_from_offset(
        &self,
        input: &str,
        initial_offset: usize,
        initial_last_token_id: Option<uuid::Uuid>,
        depth: usize,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(usize, Option<uuid::Uuid>, Vec<uuid::Uuid>, Vec<Box<Token>>), String> {
        let mut tokens: Vec<Box<Token>> = vec![];
        let mut offset = initial_offset;
        let mut child_ids: Vec<uuid::Uuid> = vec![];

        if depth > TOKEN_MATCH_TEMPLATE_FOREVER_MAX_DEPTH {
            return Ok((offset, initial_last_token_id, child_ids, tokens))
        };


        let mut last_token_id: Option<uuid::Uuid> = initial_last_token_id;
        let mut next_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();
        let mut previous_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();
        let mut parent_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();

        for template_matcher in &self.matcher {
            if offset > input.len() {
                break;
            };

            let offsetted_input = &input[offset..];
            match template_matcher {
                TokenMatchTemplateMatcher::Raw(raw) => {
                    // println!("RAW({}): {} {}", raw, input, offset);
                    if !offsetted_input.starts_with(raw) {
                        break;
                    }

                    let new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: Some(String::from(*raw)),
                        matches: HashMap::new(),
                        next_id: None,
                        previous_id: None,
                        parent_id: None,
                        children_ids: vec![],
                    });
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
                TokenMatchTemplateMatcher::Reference(reference_name) => {
                    // println!("REF({}): {} {}", reference_name, input, offset);
                    let Some(referenced_template) = token_match_templates_map.get(reference_name) else {
                        return Err(format!("Unknown reference name {} found!", reference_name))
                    };

                    let Ok((
                        referenced_template_offset,
                        referenced_last_token_id,
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

                    // Only actually store a new token if a match was found
                    if referenced_template_offset == offset {
                        continue;
                    }

                    let mut new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: None,
                        matches: HashMap::new(),
                        next_id: None,
                        previous_id: None,
                        parent_id: None,
                        children_ids: vec![],
                    });

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

                    for token in referenced_template_tokens {
                        if token.parent_id == None {
                            parent_id_mapping.insert(token.id, Some(new_token.id));
                        }
                        tokens.push(token);
                    }
                    tokens.push(new_token);

                    offset = referenced_template_offset;
                }
                TokenMatchTemplateMatcher::Regex(re) => {
                    // println!("REGEX({:?}): {} {}", re, input, offset);
                    // ref: https://stackoverflow.com/a/39239614/4115328
                    match re.captures(offsetted_input) {
                        Some(captures) => {
                            let Some(whole_match) = captures.get(0) else {
                                break;
                            };

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
                                literal: Some(String::from(whole_match.as_str())),
                                matches: matches,
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
                TokenMatchTemplateMatcher::Any(matchers) => {
                    // println!("ANY: {:?} {}", matchers, offset);
                    let mut matched_at_least_one = false;
                    for matcher in matchers {
                        let ephemeral_template = TokenMatchTemplate {
                            matcher: vec![matcher.clone()],
                        };

                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });

                        let Ok((
                            ephemeral_offset,
                            ephemeral_last_token_id,
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

                        for token in ephemeral_tokens {
                            if token.parent_id == None {
                                parent_id_mapping.insert(token.id, Some(new_token.id));
                            }
                            tokens.push(token);
                        }
                        tokens.push(new_token);

                        offset = ephemeral_offset;
                        matched_at_least_one = true;
                        break;
                    }
                    if matched_at_least_one {
                        break;
                    }
                }
                TokenMatchTemplateMatcher::RepeatCount(boxed_matcher, min_repeats, max_repeats) => {
                    // println!("REPEAT: {} {}", input, offset);
                    let mut new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut new_last_token_id = last_token_id;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate {
                        matcher: vec![*boxed_matcher.clone()],
                    };

                    // Attempt to match the ephemeral template at least `min_repeat` times:
                    for index in 0..*max_repeats {
                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });

                        let Ok((
                            ephemeral_offset,
                            ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            break;
                        };

                        // Only actually store a new token if a match was found
                        if ephemeral_offset == new_offset {
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

                        for token in ephemeral_tokens {
                            if token.parent_id == None {
                                parent_id_mapping.insert(token.id, Some(new_token.id));
                            }
                            tokens.push(token);
                        }
                        new_tokens.push(new_token);
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
                TokenMatchTemplateMatcher::RepeatForever(boxed_matcher) => {
                    // println!("FOREVER: {} {}", input, offset);
                    let mut new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut new_last_token_id = last_token_id;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate {
                        matcher: vec![*boxed_matcher.clone()],
                    };

                    loop {
                        let mut new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,
                            parent_id: None,
                            children_ids: vec![],
                        });

                        let Ok((
                            ephemeral_offset,
                            ephemeral_last_token_id,
                            ephemeral_child_ids,
                            ephemeral_tokens,
                        )) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            Some(new_token.id),
                            depth + 1,
                            token_match_templates_map,
                        ) else {
                            println!("ERR");
                            break;
                        };

                        // only actually store a new token if a match was found
                        if ephemeral_offset == new_offset {
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

                        for token in ephemeral_tokens {
                            if token.parent_id == None {
                                parent_id_mapping.insert(token.id, Some(new_token.id));
                            }
                            tokens.push(token);
                        }
                        new_tokens.push(new_token);
                    }

                    if repeat_count == 0 {
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
        }

        let parented_tokens: Vec<Box<Token>> = tokens.into_iter().map(|mut token| {
            if let Some(parent_id) = parent_id_mapping.get(&token.id) {
                token.parent_id = *parent_id;
            };

            if let Some(next_id) = next_id_mapping.get(&token.id) {
                // println!("  NEXT SET: {} {:?}", token.id, next_id);
                token.next_id = *next_id;
            };
            if let Some(previous_id) = previous_id_mapping.get(&token.id) {
                token.previous_id = *previous_id;
            };

            token
        }).collect();

        // let linked_tokens: Vec<Box<Token>> = parented_tokens.into_iter().map(|mut token| {
        //     match token.parent_id {
        //         Some(parent_id) => {
        //             if let Some(parent_token) = (&parented_tokens).iter().find(|t| t.id == parent_id) {
        //                 // for child_id in parent_token.children_ids {
        //                 //     println!("FOO: {}", child_id);
        //                 // }
        //                 // let index = parent_token.children_ids.iter().position(|n| n == &token.id).unwrap();
        //                 //
        //                 // token.previous_id = if index > 0 {
        //                 //     Some(parent_token.children_ids[index-1])
        //                 // } else { None };
        //                 // token.next_id = if index < parent_token.children_ids.len()-1 {
        //                 //     Some(parent_token.children_ids[index+1])
        //                 // } else { None };
        //             }
        //         }
        //         None => {
        //             // No parent id? Then this must be at the root
        //         }
        //     };
        //
        //     token
        // }).collect();

        Ok((offset, last_token_id, child_ids, parented_tokens))
    }
}

fn dump_inner(tokens: &Vec<Box<Token>>, child_ids: Vec<uuid::Uuid>, indent: String) {
    for child_id in child_ids {
        let Some(token) = tokens.iter().find(|t| t.id == child_id) else {
            continue;
        };
        println!("{}{:?} id:{} next:{:?} prev:{:?} \t_{}_\t==> {:?}", indent, token.template, token.id, token.next_id, token.previous_id, match &token.literal {
            Some(n) => n,
            None => "",
        }, token.matches);
        dump_inner(tokens, token.children_ids.clone(), format!("{}  ", indent));
    }
}
fn dump(head_id: uuid::Uuid, tokens: &Vec<Box<Token>>) {
    dump_inner(tokens, vec![head_id], "".to_string());
}

fn stringify(head_id: uuid::Uuid, tokens: &Vec<Box<Token>>) -> String {
    let mut result = String::from("");
    let mut pointer_id = head_id;
    loop {
        let Some(pointer) = tokens.iter().find(|t| t.id == pointer_id) else {
            continue;
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







fn main() {
    let mut token_match_templates_map = HashMap::new();
    token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::RepeatForever(Box::new(
            TokenMatchTemplateMatcher::Any(vec![
                TokenMatchTemplateMatcher::Reference("Whitespace"),
                TokenMatchTemplateMatcher::Reference("Declaration"),
                TokenMatchTemplateMatcher::Reference("Block"),
            ]),
        )),
    ]));

    token_match_templates_map.insert("Block", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Raw("{"),
        TokenMatchTemplateMatcher::RepeatForever(Box::new(
            TokenMatchTemplateMatcher::Any(vec![
                TokenMatchTemplateMatcher::Reference("Statement"),
                TokenMatchTemplateMatcher::Reference("Whitespace"),
            ]),
        )),
        TokenMatchTemplateMatcher::Raw("}"),
    ]));

    token_match_templates_map.insert("Statement", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Any(vec![
            TokenMatchTemplateMatcher::Reference("Declaration"),
            TokenMatchTemplateMatcher::Reference("Expression"),
        ]),
    ]));

    token_match_templates_map.insert("Declaration", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Raw("let"),
        TokenMatchTemplateMatcher::Reference("Whitespace"),
        TokenMatchTemplateMatcher::Reference("Identifier"),
        TokenMatchTemplateMatcher::RepeatCount(Box::new(
            TokenMatchTemplateMatcher::Reference("Whitespace"),
        ), 0, 1),
        TokenMatchTemplateMatcher::Raw("="),
        TokenMatchTemplateMatcher::RepeatCount(Box::new(
            TokenMatchTemplateMatcher::Reference("Whitespace"),
        ), 0, 1),
        TokenMatchTemplateMatcher::Reference("Expression"),
    ]));

    token_match_templates_map.insert("StringLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Regex(
            Regex::new(r"^(?<literal>'[^']*')").unwrap(),
        ),
    ]));
    token_match_templates_map.insert("NumberLiteral", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Regex(
            Regex::new(r"^(?<literal>[0-9]+)").unwrap(),
        ),
    ]));
    token_match_templates_map.insert("Variable", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Reference("Identifier"),
    ]));

    token_match_templates_map.insert("Expression", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Any(vec![
            TokenMatchTemplateMatcher::Reference("StringLiteral"),
            TokenMatchTemplateMatcher::Reference("NumberLiteral"),
            TokenMatchTemplateMatcher::Reference("Variable"),
            TokenMatchTemplateMatcher::Reference("Block"),
        ]),
    ]));

    token_match_templates_map.insert("Identifier", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Regex(
            Regex::new(r"^(?<value>[a-zA-Z](?:[a-zA-Z0-9_\$])*)").unwrap(),
        ),
    ]));

    token_match_templates_map.insert("Whitespace", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Regex(
            Regex::new(r"^\s+").unwrap(),
        ),
    ]));

    // token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
    //     TokenMatchTemplateMatcher::RepeatCount(Box::new(
    //         TokenMatchTemplateMatcher::RepeatForever(Box::new(
    //             TokenMatchTemplateMatcher::Any(vec![
    //                 TokenMatchTemplateMatcher::Raw("1"),
    //             ]),
    //         )),
    //     ), 1, 3),
    // ]));

    let Some(all_template) = token_match_templates_map.get("All") else {
        panic!("No 'All' template found!");
    };

    let input = "
let b = 'fff'
{
    {
        let a = 'aaa'
    }
}";

    // let input = "11";

    match all_template.consume_from_start(input, &token_match_templates_map) {
        Ok((offset, last_token_id, child_ids, tokens)) => {
            // println!("RESULT: {:?} {:?}", offset, tokens);
            println!("Offset: {}\nInput:\n{}\n---\n", offset, input);

            println!("=========");
            println!("= TOKENS:");
            println!("=========");

            for child_id in &child_ids {
                dump(*child_id, &tokens);
                println!("---------");
            }

            println!("=========");
            println!("= STRINGS:");
            println!("=========");

            println!("{}", stringify(child_ids[0], &tokens));
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}
