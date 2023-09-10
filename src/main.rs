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

impl TokenMatchTemplate {
    fn new(matcher: Vec<TokenMatchTemplateMatcher>) -> TokenMatchTemplate {
        TokenMatchTemplate { matcher: matcher }
    }
    fn consume_from_start(
        &self,
        input: &str,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(usize, Vec<uuid::Uuid>, Vec<Box<Token>>), String> {
        self.consume_from_offset(input, 0, token_match_templates_map)
    }
    fn consume_from_offset(
        &self,
        input: &str,
        initial_offset: usize,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(usize, Vec<uuid::Uuid>, Vec<Box<Token>>), String> {
        let mut tokens: Vec<Box<Token>> = vec![];
        let mut offset = initial_offset;
        let mut child_ids: Vec<uuid::Uuid> = vec![];

        let mut last_token_id: Option<uuid::Uuid> = None;
        let mut next_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();
        let mut previous_id_mapping: HashMap<uuid::Uuid, Option<uuid::Uuid>> = HashMap::new();

        for template_matcher in &self.matcher {
            if offset > input.len() {
                break;
            };

            let offsetted_input = &input[offset..];
            match template_matcher {
                TokenMatchTemplateMatcher::Raw(raw) => {
                    println!("RAW({}): {} {}", raw, input, offset);
                    if !offsetted_input.starts_with(raw) {
                        return Ok((initial_offset, vec![], vec![]))
                    }

                    let new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: Some(String::from(*raw)),
                        matches: HashMap::new(),
                        next_id: None,
                        previous_id: None,
                        children_ids: vec![],
                    });
                    child_ids.push(new_token.id);

                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }
                    last_token_id = Some(new_token.id);

                    tokens.push(new_token);

                    offset += raw.len();
                }
                TokenMatchTemplateMatcher::Reference(reference_name) => {
                    println!("REF({}): {} {}", reference_name, input, offset);
                    let Some(referenced_template) = token_match_templates_map.get(reference_name) else {
                        return Err(format!("Unknown reference name {} found!", reference_name))
                    };

                    let Ok(
                        (referenced_template_offset, referenced_child_ids, referenced_template_tokens)
                    ) = referenced_template.consume_from_offset(
                        input,
                        offset,
                        token_match_templates_map,
                    ) else {
                        continue;
                    };

                    // Only actually store a new token if a match was found
                    if referenced_template_offset == offset {
                        continue;
                    }

                    let new_token = Box::new(Token {
                        id: Uuid::new_v4(),
                        template: template_matcher.clone(),
                        literal: None,
                        matches: HashMap::new(),
                        next_id: None,
                        previous_id: None,//last_token,
                        children_ids: referenced_child_ids,
                    });
                    child_ids.push(new_token.id);

                    previous_id_mapping.insert(new_token.id, last_token_id);
                    if let Some(last_token_id_unwrapped) = last_token_id {
                        next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                    }
                    last_token_id = Some(new_token.id);

                    for token in referenced_template_tokens {
                        tokens.push(token);
                    }
                    tokens.push(new_token);

                    offset = referenced_template_offset;
                }
                TokenMatchTemplateMatcher::Regex(re) => {
                    println!("REGEX({:?}): {} {}", re, input, offset);
                    // ref: https://stackoverflow.com/a/39239614/4115328
                    match re.captures(offsetted_input) {
                        Some(captures) => {
                            let Some(whole_match) = captures.get(0) else {
                                continue;
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
                                previous_id: None,//last_token,
                                children_ids: vec![],
                            });
                            child_ids.push(new_token.id);

                            previous_id_mapping.insert(new_token.id, last_token_id);
                            if let Some(last_token_id_unwrapped) = last_token_id {
                                next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                            }
                            last_token_id = Some(new_token.id);

                            tokens.push(new_token);

                            offset += whole_match.len();
                        }
                        None => {
                            continue;
                        }
                    }
                }
                TokenMatchTemplateMatcher::Any(matchers) => {
                    println!("ANY: {:?} {}", matchers, offset);
                    for matcher in matchers {
                        let ephemeral_template = TokenMatchTemplate {
                            matcher: vec![matcher.clone()],
                        };

                        let Ok(
                            (ephemeral_offset, ephemeral_child_ids, ephemeral_tokens)
                        ) = ephemeral_template.consume_from_offset(
                            input,
                            offset,
                            token_match_templates_map,
                        ) else {
                            continue;
                        };

                        println!("OUTPUT: {:?} {} {}", matchers, offset, ephemeral_offset);

                        // Only actually store a new token if a match was found
                        if ephemeral_offset == offset {
                            continue;
                        }

                        let new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,//last_token,
                            children_ids: ephemeral_child_ids,
                        });
                        child_ids.push(new_token.id);

                        previous_id_mapping.insert(new_token.id, last_token_id);
                        if let Some(last_token_id_unwrapped) = last_token_id {
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }
                        last_token_id = Some(new_token.id);

                        for token in ephemeral_tokens {
                            tokens.push(token);
                        }
                        tokens.push(new_token);

                        offset = ephemeral_offset;
                        break;
                    }
                }
                TokenMatchTemplateMatcher::RepeatCount(boxed_matcher, min_repeats, max_repeats) => {
                    println!("REPEAT: {} {}", input, offset);
                    let mut new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate {
                        matcher: vec![*boxed_matcher.clone()],
                    };

                    // Attempt to match the ephemeral template at least `min_repeat` times:
                    for index in 0..*max_repeats {
                        let Ok(
                            (ephemeral_offset, ephemeral_child_ids, ephemeral_tokens)
                        ) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
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

                        let new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,//last_token,
                            children_ids: ephemeral_child_ids,
                        });
                        child_ids.push(new_token.id);

                        previous_id_mapping.insert(new_token.id, last_token_id);
                        if let Some(last_token_id_unwrapped) = last_token_id {
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }
                        last_token_id = Some(new_token.id);

                        for token in ephemeral_tokens {
                            tokens.push(token);
                        }
                        new_tokens.push(new_token);
                    }

                    if repeat_count == 0 {
                        continue;
                    }
                    if repeat_count < *min_repeats {
                        continue;
                    }
                    if repeat_count > *max_repeats {
                        continue;
                    }
                    println!("REPEATRESULT: {} {} {}", repeat_count, min_repeats, max_repeats);

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                }
                TokenMatchTemplateMatcher::RepeatForever(boxed_matcher) => {
                    println!("FOREVER: {} {}", input, offset);
                    let mut new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate {
                        matcher: vec![*boxed_matcher.clone()],
                    };

                    loop {
                        let Ok(
                            (ephemeral_offset, ephemeral_child_ids, ephemeral_tokens)
                        ) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            token_match_templates_map,
                        ) else {
                            break;
                        };

                        // only actually store a new token if a match was found
                        if ephemeral_offset == new_offset {
                            break;
                        }

                        // Another match was found, so store another generated token
                        repeat_count += 1;
                        new_offset = ephemeral_offset;

                        let new_token = Box::new(Token {
                            id: Uuid::new_v4(),
                            template: template_matcher.clone(),
                            literal: None,
                            matches: HashMap::new(),
                            next_id: None,
                            previous_id: None,//last_token,
                            children_ids: ephemeral_child_ids,
                        });
                        child_ids.push(new_token.id);

                        previous_id_mapping.insert(new_token.id, last_token_id);
                        if let Some(last_token_id_unwrapped) = last_token_id {
                            next_id_mapping.insert(last_token_id_unwrapped, Some(new_token.id));
                        }
                        last_token_id = Some(new_token.id);

                        for token in ephemeral_tokens {
                            tokens.push(token);
                        }
                        new_tokens.push(new_token);
                    }

                    if repeat_count == 0 {
                        continue;
                    }

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                }
            }
        }

        let new_tokens = tokens.into_iter().map(|mut token| {
            if let Some(next_id) = next_id_mapping.get(&token.id) {
                token.next_id = *next_id;
            };
            if let Some(previous_id) = previous_id_mapping.get(&token.id) {
                token.previous_id = *previous_id;
            };
            token
        }).collect();

        Ok((offset, child_ids, new_tokens))
    }
}

fn dump_inner(tokens: &Vec<Box<Token>>, child_ids: Vec<uuid::Uuid>, indent: String) {
    for child_id in child_ids {
        let Some(token) = tokens.iter().find(|t| t.id == child_id) else {
            continue;
        };
        println!("{}{:?}\t_{}_\t==> {:?}", indent, token.template, match &token.literal {
            Some(n) => n,
            None => "",
        }, token.matches);
        dump_inner(tokens, token.children_ids.clone(), format!("{}  ", indent));
    }
}
fn dump(head_id: uuid::Uuid, tokens: Vec<Box<Token>>) {
    dump_inner(&tokens, vec![head_id], "".to_string());
}

// fn dump(tokens: Vec<Box<Token>>) {
// }







fn main() {
    let mut token_match_templates_map = HashMap::new();
    token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Any(vec![
            TokenMatchTemplateMatcher::Reference("Declaration"),
            TokenMatchTemplateMatcher::Reference("Block"),
        ]),
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
        TokenMatchTemplateMatcher::Reference("Whitespace"),
        TokenMatchTemplateMatcher::Raw("="),
        TokenMatchTemplateMatcher::Reference("Whitespace"),
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


    let Some(all_template) = token_match_templates_map.get("All") else {
        panic!("No 'All' template found!");
    };

    let input ="{
        {
            let a = 'aaa'
        }
    }";

    // let input ="let a = 'aaa'";

    match all_template.consume_from_start(input, &token_match_templates_map) {
        Ok((offset, child_ids, tokens)) => {
            // println!("RESULT: {:?} {:?}", offset, tokens);
            println!("Offset: {}\nInput:\n{}\n---\n", offset, input);
            dump(child_ids[0], tokens);
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}
