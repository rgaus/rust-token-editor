use std::collections::HashMap;
use regex::Regex;

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
    template: TokenMatchTemplateMatcher,
    matches: HashMap<String, TokenMatch>,

    next: Option<Box<Token>>,
    previous: Option<Box<Token>>,
    children: Vec<Box<Token>>,
}

#[derive(Debug)]
#[derive(Clone)]
enum TokenMatchTemplateMatcher {
    Raw(&'static str),
    Reference(&'static str),
    Regex(regex::Regex),
    Any(Vec<TokenMatchTemplateMatcher>),
    Repeat(Box<TokenMatchTemplateMatcher>, usize, usize),
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
    fn consume_from_offset(
        &self,
        input: &str,
        initial_offset: usize,
        token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
    ) -> Result<(usize, Vec<Box<Token>>), String> {
        let mut tokens: Vec<Box<Token>> = vec![];
        let mut offset = initial_offset;

        // Store a reference to the last parsed token so that when adding a new token, this can be
        // assigned as the "previous" token value
        let mut last_token: Option<&Box<Token>> = None;

        for template_matcher in &self.matcher {
            if offset > input.len() {
                break;
            }

            let offsetted_input = &input[offset..];
            match template_matcher {
                TokenMatchTemplateMatcher::Raw(raw) => {
                    if !offsetted_input.starts_with(raw) {
                        return Ok((0, vec![]))
                    }

                    let new_token = Box::new(Token {
                        template: template_matcher.clone(),
                        matches: HashMap::new(),
                        next: None,
                        previous: None,//last_token,
                        children: vec![],
                    });
                    last_token = Some(&new_token);
                    tokens.push(new_token);

                    offset += raw.len();
                }
                TokenMatchTemplateMatcher::Reference(reference_name) => {
                    let Some(referenced_template) = token_match_templates_map.get(reference_name) else {
                        return Err(format!("Unknown reference name {} found!", reference_name))
                    };

                    let Ok(
                        (referenced_template_offset, referenced_template_tokens)
                    ) = referenced_template.consume_from_offset(
                        input,
                        offset,
                        token_match_templates_map,
                    ) else {
                        continue;
                    };
                    println!("FOO: {} {} {}", offset, referenced_template_offset, reference_name);

                    // Only actually store a new token if a match was found
                    if referenced_template_offset == offset {
                        continue;
                    }

                    let new_token = Box::new(Token {
                        template: template_matcher.clone(),
                        matches: HashMap::new(),
                        next: None,
                        previous: None,//last_token,
                        children: referenced_template_tokens,
                    });
                    last_token = Some(&new_token);
                    tokens.push(new_token);

                    offset = referenced_template_offset;
                }
                TokenMatchTemplateMatcher::Regex(re) => {
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
                                template: template_matcher.clone(),
                                matches: matches,
                                next: None,
                                previous: None,//last_token,
                                children: vec![],
                            });
                            last_token = Some(&new_token);
                            tokens.push(new_token);

                            offset += whole_match.len();
                        }
                        None => {
                            continue;
                        }
                    }
                }
                TokenMatchTemplateMatcher::Any(matchers) => {
                    for matcher in matchers {
                        let ephemeral_template = TokenMatchTemplate {
                            matcher: vec![matcher.clone()],
                        };

                        println!("BAR: {} {}", input, offset);
                        let Ok(
                            (ephemeral_offset, ephemeral_tokens)
                        ) = ephemeral_template.consume_from_offset(
                            input,
                            offset,
                            token_match_templates_map,
                        ) else {
                            continue;
                        };

                        // Only actually store a new token if a match was found
                        if ephemeral_offset == offset {
                            continue;
                        }

                        let new_token = Box::new(Token {
                            template: template_matcher.clone(),
                            matches: HashMap::new(),
                            next: None,
                            previous: None,//last_token,
                            children: ephemeral_tokens,
                        });
                        last_token = Some(&new_token);
                        tokens.push(new_token);

                        offset = ephemeral_offset;
                        break;
                    }
                }
                TokenMatchTemplateMatcher::Repeat(boxed_matcher, min_repeats, max_repeats) => {
                    let mut new_tokens: Vec<Box<Token>> = vec![];
                    let mut new_offset = offset;
                    let mut repeat_count: usize = 0;

                    let ephemeral_template = TokenMatchTemplate {
                        matcher: vec![*boxed_matcher.clone()],
                    };

                    // Attempt to match the ephemeral template at least `min_repeat` times:
                    for index in 0..*max_repeats {
                        println!("BAZ: {} {}", input, offset);
                        let Ok(
                            (ephemeral_offset, ephemeral_tokens)
                        ) = ephemeral_template.consume_from_offset(
                            input,
                            new_offset,
                            token_match_templates_map,
                        ) else {
                            break;
                        };

                        // Only actually store a new token if a match was found
                        if ephemeral_offset == 0 {
                            break;
                        }

                        // Another match was found, so store another generated token
                        repeat_count += 1;
                        new_offset += ephemeral_offset;

                        let new_token = Box::new(Token {
                            template: template_matcher.clone(),
                            matches: HashMap::new(),
                            next: None,
                            previous: None,//last_token,
                            children: ephemeral_tokens,
                        });
                        last_token = Some(&new_token);
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

                    // Update the main values with the local cached values
                    for new_token in new_tokens {
                        tokens.push(new_token);
                    }
                    offset = new_offset;
                }
            }
        }

        Ok((offset, tokens))
    }
}

fn dump_inner(tokens: Vec<Box<Token>>, indent: String) {
    for token in tokens {
        println!("{}{:?} ==> {:?}", indent, token.template, token.matches);
        dump_inner(token.children, format!("{}  ", indent));
    }
}
fn dump(tokens: Vec<Box<Token>>) {
    dump_inner(tokens, "".to_string());
}

fn parse(input: &str) {
    let mut token_match_templates_map = HashMap::new();
    token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
        TokenMatchTemplateMatcher::Any(vec![
            TokenMatchTemplateMatcher::Reference("Declaration"),
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

    match all_template.consume_from_offset(input, 0, &token_match_templates_map) {
        Ok((offset, tokens)) => {
            // println!("RESULT: {:?} {:?}", offset, tokens);
            println!("Offset: {}\nInput:\n{}\n---\n", offset, input);
            dump(tokens);
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}







fn main() {
    parse("let a = 'aaa'");
}
