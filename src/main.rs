use std::collections::HashMap;
use regex::Regex;
use colored::Colorize;

mod token;
use token::*;

mod token_match_template;
use token_match_template::*;


fn dump_inner(tokens: &Vec<Box<Token>>, child_ids: Vec<uuid::Uuid>, indent: String) {
    for child_id in child_ids {
        let Some(token) = tokens.iter().find(|t| t.id == child_id) else {
            println!("CHILD NOT FOUND! {}", child_id);
            continue;
        };
        println!("{}{:?} id:{} next:{:?} prev:{:?} parent:{:?} \t_{}_\t==> {:?}", indent, token.template, token.id, token.next_id, token.previous_id, token.parent_id, match &token.literal {
            Some(n) => n,
            None => "",
        }, token.matches);
        // println!("{}{:?} \t_{}_\t==> {:?} {:?}", indent, token.template, match &token.literal {
        //     Some(n) => n,
        //     None => "",
        // }, token.matches, token.effects);
        // println!("{}{:?} => {:?}", indent, token.template, token.effects);
        dump_inner(tokens, token.children_ids.clone(), format!("{}  ", indent));
    }
}
fn dump(head_id: uuid::Uuid, tokens: &Vec<Box<Token>>) {
    dump_inner(tokens, vec![head_id], "".to_string());
}

fn stringify_colors(
    head_id: uuid::Uuid,
    tokens_collection: &TokensCollection,
) -> String {
    let mut result = String::from("");
    let mut pointer_id = head_id;

    let color_sequence = vec![
        "bright_green",
        "green",
        "bright_blue",
        "blue",
        "bright_magenta",
        "magenta",
        "bright_cyan",
        "cyan",
        "bright_red",
        "red",
        "bright_yellow",
        "yellow",
        "bright_purple",
        "purple",
    ];
    let mut color_sequence_index = 0;

    let mut colors = vec![];

    let mut last_depth = 0;

    loop {
        let Some(mut pointer) = tokens_collection.get_by_id(pointer_id) else {
            println!("BAD POINTER! {:?}", pointer_id);
            break;
        };

        let depth = pointer.depth(&tokens_collection);
        if depth <= last_depth {
            while depth < last_depth {
                colors.pop();
                last_depth -= 1;
            }
        } else {
            last_depth = depth;
            let color = color_sequence.get(
                color_sequence_index % color_sequence.len()
            ).unwrap_or(&"normal");
            colors.push(*color);

            color_sequence_index += 1;
        };

        if let Some(literal_text) = &pointer.literal {
            let color = colors.last().unwrap_or(&"normal");
            let colored_literal_text = match *color {
                "red" => literal_text.clone().red(),
                "green" => literal_text.clone().green(),
                "blue" => literal_text.clone().blue(),
                "yellow" => literal_text.clone().yellow(),
                "cyan" => literal_text.clone().cyan(),
                "magenta" => literal_text.clone().magenta(),
                "purple" => literal_text.clone().purple(),
                "bright_red" => literal_text.clone().bright_red(),
                "bright_green" => literal_text.clone().bright_green(),
                "bright_blue" => literal_text.clone().bright_blue(),
                "bright_yellow" => literal_text.clone().bright_yellow(),
                "bright_cyan" => literal_text.clone().bright_cyan(),
                "bright_magenta" => literal_text.clone().bright_magenta(),
                "bright_purple" => literal_text.clone().bright_purple(),
                _ | "normal" => literal_text.clone().normal(),
            };
            // result = format!("{}{}", result, colored_literal_text);
            println!("{}", colored_literal_text);
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
        // on_leave: Some(|token, tokens_collection| {
        //     let Some(parent_id) = token.parent_id else {
        //         return;
        //     };
        //     println!("DETAILS ID: {:?}", parent_id);
        //     let Some(parent_token) = tokens_collection.tokens.iter().find(|t| t.id == parent_id) else {
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
        on_leave: Some(|token, tokens_collection| {
            let Some(TokenEffect::DeclareIdentifier(identifier)) = token.find_child_effect(tokens_collection, |e| {
                if let TokenEffect::DeclareIdentifier(_) = e { true } else { false }
            }) else { return; };

            let Some(TokenEffect::DeclareExpression(expression)) = token.find_child_effect(tokens_collection, |e| {
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
                on_leave: Some(|token, tokens_collection| {
                    let Some(parent_id) = token.parent_id else {
                        return;
                    };
                    tokens_collection.get_by_id_mut(parent_id, |parent| {
                        parent.effects.push(TokenEffect::DeclareExpression(
                            token.matches.get("literal").unwrap().string.clone(),
                        ));
                    });
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

    token_match_templates_map.insert("ArrayLiteral", TokenMatchTemplate::new_with_events(vec![
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
    ], TokenEvents {
        on_enter: None,
        on_leave: Some(|token, tokens_collection| {
            let entries = token.find_deep_children(tokens_collection, 4, |token| match token.template {
                TokenMatchTemplateMatcher::Reference("ArrayLiteralEntry", None) => true,
                _ => false,
            });

            let mut expression_literals = vec![];
            for entry in entries {
                for effect in &entry.effects {
                    let TokenEffect::DeclareExpression(literal) = effect else {
                        continue;
                    };
                    expression_literals.push(String::from(literal));
                }
            };
            token.effects.push(TokenEffect::DeclareExpression(
                format!("[{}]", expression_literals.join(", "))
            ));
        }),
    }));
    token_match_templates_map.insert("ArrayLiteralEntry", TokenMatchTemplate::new_with_events(vec![
        TokenMatchTemplateMatcher::reference("Expression"),
    ], TokenEvents {
        on_enter: None,
        on_leave: Some(|token, tokens_collection| {
            let Some(expression) = token.find_child_effect(tokens_collection, |e| match e {
                TokenEffect::DeclareExpression(_) => true,
                _ => false,
            }) else { return; };

            token.effects.push(expression.clone());
        }),
    }));

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
        on_leave: Some(|token, tokens_collection| {
            let Some(next) = token.next(tokens_collection) else {
                return;
            };

            let Some(expression) = next.find_child_effect(tokens_collection, |e| {
                if let TokenEffect::DeclareExpression(_) = e { true } else { false }
            }) else { return; };

            token.effects.push(expression.clone());
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
                on_leave: Some(|token, tokens_collection| {
                    let Some(parent_id) = token.parent_id else {
                        return;
                    };
                    tokens_collection.get_by_id_mut(parent_id, |parent| {
                        parent.effects.push(TokenEffect::DeclareIdentifier(
                            token.matches.get("value").unwrap().string.clone(),
                        ));
                    });
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

    // let input = "{let a = ['aaa', ['cc', 'bbb']]}";
    let input = "{let a = ['aaa', [@'cc', 'bbb']]}";
    // let input = "let a = ['aaa'@, 1]";
    // let input = "let @a = 'fo'@@";
    // let input = "456";

    // let input = "1aa1bb";

    // match all_template.consume_from_start(input, false, &token_match_templates_map) {
    match all_template.consume_from_start(input, true, &token_match_templates_map) {
        Ok((match_status, offset, _last_token_id, child_ids, mut tokens_collection)) => {
            println!("RESULT: {:?} {:?}", offset, match_status);
            println!("Offset: {}\nInput:\n{}\n---\n", offset, input);

            println!("=========");
            println!("= TOKENS: {} {}", tokens_collection.tokens.len(), input.len());
            println!("=========");

            for child_id in &child_ids {
                dump(*child_id, &tokens_collection.tokens);
                println!("---------");
            }

            println!("=========");
            println!("= STRINGS:");
            println!("=========");

            if !child_ids.is_empty() {
                {
                    // println!("{}", tokens_collection.stringify());
                    println!("{}", stringify_colors(child_ids[0], &tokens_collection));
                }

                // println!("RESULT: {:?}", tokens_collection.get_by_offset(13));
                //
                // println!("=========");
                // println!("= MUTATION:");
                // println!("=========");
                //
                // let token_id = {
                //     let (token, _) = tokens_collection.get_by_offset(13).unwrap();
                //     println!("TOK: {:?}", token);
                //     token.id
                // };
                //
                // // let token_id = {
                // //     let top = tokens_collection.get_by_id(child_ids[0]).unwrap();
                // //     let token = top.find_deep_child(&tokens_collection, 100, |token| match token {
                // //         Token { literal: Some(text), .. } if text == "'aaa'" => true,
                // //         _ => false,
                // //     }).unwrap();
                // //     token.id
                // // };
                //
                // let new_subtree_token_id = tokens_collection.change_token_literal_text(
                //     token_id,
                //     "aba".to_string(),
                //     &token_match_templates_map,
                // ).unwrap();
                //
                // println!("--------- {}", new_subtree_token_id);
                // // dump(new_subtree_token_id, &tokens_collection.tokens);
                // // for child_id in &child_ids {
                // //     dump(*child_id, &tokens_collection.tokens);
                // //     println!("---------");
                // // }
                // {
                //     // println!("{}", tokens_collection.stringify_to_end(child_ids[0]));
                //     println!("{}", stringify_colors(child_ids[0], &tokens_collection));
                // }
                //
                // println!("RESULT: {:?}", tokens_collection.get_by_offset(13));
            }
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}



#[cfg(test)]
mod test_parsing {
    use super::*;

    mod mini_language_twelve {
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

        mod exact_parsing {
            use super::*;

            #[test]
            fn it_fully_parses_1() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("1", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 1); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 3); // tokens_collection
                assert_eq!(result.4.stringify(), "1");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_fully_parses_12() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("12", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 2); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 6); // tokens_collection
                assert_eq!(result.4.stringify(), "12");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_fully_parses_112() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("112", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 3); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 9); // tokens_collection
                assert_eq!(result.4.stringify(), "112");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_fully_parses_1112() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("1112", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 4); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 12); // tokens_collection
                assert_eq!(result.4.stringify(), "1112");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_starts_to_parse_1112aa() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("1112aa", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 4); // offset - NOTE: not the whole string!
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 12); // tokens_collection
                assert_eq!(result.4.stringify(), "1112");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_parses_11112_as_just_1() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("11112", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::FullParse); // status
                assert_eq!(result.1, 1); // offset - NOTE: not the whole string!
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 3); // tokens_collection
                assert_eq!(result.4.stringify(), "1");
                // dump(result.3[0], &result.4.tokens);
            }

            #[test]
            fn it_doesnt_parse_333() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("333", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::Failed); // status
                assert_eq!(result.1, 0); // offset - NOTE: it parsed no characters!
                assert_eq!(result.3.len(), 0); // child_ids
                assert_eq!(result.4.tokens.len(), 0); // tokens_collection
                // dump(result.3[04, &result.4.tokens);
            }

            #[test]
            fn it_doesnt_parse_empty_string() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("", false, &template_map).unwrap();
                assert_eq!(result.0, TokenParseStatus::Failed); // status
                assert_eq!(result.1, 0); // offset - NOTE: not the whole string!
                assert_eq!(result.3.len(), 0); // child_ids
                assert_eq!(result.4.tokens.len(), 0); // tokens_collection
                // dump(result.3[04, &result.4.tokens);
            }
        }

        mod loose_parsing {
            use super::*;

            #[test]
            fn it_loosely_parses_1_with_prefix() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("a1", true, &template_map).unwrap();
                // dump(result.3[0], &result.4.tokens);
                assert_eq!(result.0, TokenParseStatus::PartialParse(2, Some(0))); // status
                assert_eq!(result.1, 2); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                assert_eq!(result.4.tokens.len(), 6); // tokens_collection
                assert_eq!(result.4.stringify(), "a1");
            }

            #[test]
            fn it_loosely_parses_1_with_suffix() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("1a", true, &template_map).unwrap();
                dump(result.3[0], &result.4.tokens);
                assert_eq!(result.0, TokenParseStatus::PartialParse(2, Some(1))); // status
                assert_eq!(result.1, 2); // offset
                assert_eq!(result.3.len(), 2); // child_ids
                assert_eq!(result.4.tokens.len(), 4); // tokens_collection
                assert_eq!(result.4.stringify(), "1a");
            }

            #[test]
            fn it_loosely_parses_112_with_garbage_in_the_middle() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("AA11BB2CC", true, &template_map).unwrap();
                // dump(result.3[0], &result.4.tokens);
                assert_eq!(result.0, TokenParseStatus::PartialParse(9, Some(0))); // status
                assert_eq!(result.1, 9); // offset
                assert_eq!(result.3.len(), 2); // child_ids
                assert_eq!(result.4.tokens.len(), 12); // tokens_collection
                assert_eq!(result.4.stringify(), "AA11BB2CC");
            }

            #[test]
            fn it_loosely_parses_total_garbage() {
                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("TOTAL GARBAGE", true, &template_map).unwrap();
                // dump(result.3[0], &result.4.tokens);
                assert_eq!(result.0, TokenParseStatus::PartialParse(13, Some(0))); // status
                assert_eq!(result.1, 13); // offset
                assert_eq!(result.3.len(), 1); // child_ids
                // NOTE: This should parse as a single TokenMatchTemplateMatcher::Skipped token
                // with the entire contents inside
                assert_eq!(result.4.tokens.len(), 1); // tokens_collection
                assert_eq!(result.4.stringify(), "TOTAL GARBAGE");
            }

            #[test]
            fn it_loosely_parses_111112_as_1_with_skipped_chars_afterward() {
                // NOTE: the reason it parses this way ("1" "11112", where "11112" is skipped)
                // versus "111" "11" "2" (where "11" is skipped) is because the "One" path fully
                // matches, and that is prioritized over a path that partially matches / contains
                // skipped tokens ("Twelve"), even if that path is longer!

                let template_map = initialize_mini_language_twelve();
                let all_template = template_map.get("All").unwrap();

                let result = all_template.consume_from_start("111112", true, &template_map).unwrap();
                // dump(result.3[0], &result.4.tokens);
                assert_eq!(result.0, TokenParseStatus::PartialParse(6, Some(1))); // status
                assert_eq!(result.1, 6); // offset
                assert_eq!(result.3.len(), 2); // child_ids
                assert_eq!(result.4.tokens.len(), 4); // tokens_collection
                assert_eq!(result.4.stringify(), "111112");
            }
        }
    }

    mod mini_language_math {
        use super::*;

        // This mini language implements a four function math expression parser with parenthesis
        // for grouping.
        fn initialize_mini_language_math() -> HashMap<&'static str, TokenMatchTemplate> {
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

            token_match_templates_map
        }

        #[test]
        fn it_fully_parses_1_plus_1() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+1", false, &template_map).unwrap();
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 3); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 11); // tokens_collection
            assert_eq!(result.4.stringify(), "1+1");
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_1_plus_negative_1() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+-1", false, &template_map).unwrap();
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 4); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 11); // tokens_collection
            assert_eq!(result.4.stringify(), "1+-1");
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_1_plus_quantity_5_times_6() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+(5*6)", false, &template_map).unwrap();
            // dump(result.3[0], &result.4.tokens);
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 7); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 23); // tokens_collection
            assert_eq!(result.4.stringify(), "1+(5*6)");
        }

        #[test]
        fn it_partially_parses_1_plus_as_just_one() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+", false, &template_map).unwrap();
            // dump(result.3[0], &result.4.tokens);
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 1); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 3); // tokens_collection

            // NOTE: `store_non_parsable_chars` is not set! So the non matching last character
            // should not be included
            assert_eq!(result.4.stringify(), "1");
        }
    }

    mod mini_language_looping_expressions {
        use super::*;

        // This mini language implements a series of strings and numbers with whitespace in between
        fn initialize_mini_language_looping_expressions() -> HashMap<&'static str, TokenMatchTemplate> {
            let mut token_match_templates_map = HashMap::new();
            token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::repeat_zero_to_forever(Box::new(
                    TokenMatchTemplateMatcher::any(vec![
                        TokenMatchTemplateMatcher::reference("Whitespace"),
                        TokenMatchTemplateMatcher::reference("String"),
                        TokenMatchTemplateMatcher::reference("PositiveInteger"),
                    ]),
                )),
            ]));
            token_match_templates_map.insert("String", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::raw("'"),
                TokenMatchTemplateMatcher::regex(
                    Regex::new(r"^(?<literal>[^']*)").unwrap(),
                ),
                TokenMatchTemplateMatcher::raw("'"),
            ]));
            token_match_templates_map.insert("PositiveInteger", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::regex(
                    Regex::new(r"^(?<value>[0-9]+)").unwrap(),
                ),
            ]));
            token_match_templates_map.insert("Whitespace", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::regex(
                    Regex::new(r"^\s+").unwrap(),
                ),
            ]));

            token_match_templates_map
        }

        #[test]
        fn it_fully_parses_abc_enter_123() {
            let template_map = initialize_mini_language_looping_expressions();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("'abc'\n123", false, &template_map).unwrap();
            dump(result.3[0], &result.4.tokens);
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 9); // offset
            assert_eq!(result.3.len(), 3); // child_ids
            assert_eq!(result.4.tokens.len(), 14); // tokens_collection
            assert_eq!(result.4.stringify(), "'abc'\n123");
        }

        #[test]
        fn it_fully_parses_quote_123_quote_as_string() {
            let template_map = initialize_mini_language_looping_expressions();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("'123'", false, &template_map).unwrap();
            dump(result.3[0], &result.4.tokens);
            assert_eq!(result.0, TokenParseStatus::FullParse); // status
            assert_eq!(result.1, 5); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 6); // tokens_collection
            assert_eq!(result.4.stringify(), "'123'");

            // Make sure that there is a String somewhere in the token collection
            let root_node = result.4.get_first_root_node().unwrap();
            let matching_node = root_node.find_deep_child(&result.4, 100, |token| match token {
                Token { template: TokenMatchTemplateMatcher::Reference("String", _), .. } => true,
                _ => false,
            });
            assert!(matching_node.is_some());
        }
    }

    #[test]
    fn it_fully_parses_optional_whitespace() {
        let template_map = {
            let mut token_match_templates_map = HashMap::new();
            token_match_templates_map.insert("All", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::repeat_zero_to_forever(Box::new(
                    TokenMatchTemplateMatcher::any(vec![
                        TokenMatchTemplateMatcher::reference("A"),
                        TokenMatchTemplateMatcher::reference("B"),
                    ]),
                )),
            ]));
            token_match_templates_map.insert("A", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::raw("A"),
                TokenMatchTemplateMatcher::reference("Whitespace"),
                TokenMatchTemplateMatcher::raw("A"),
            ]));
            token_match_templates_map.insert("B", TokenMatchTemplate::new(vec![
                TokenMatchTemplateMatcher::raw("B"),
                TokenMatchTemplateMatcher::reference("OptionalWhitespace"),
                TokenMatchTemplateMatcher::raw("B"),
            ]));

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

            token_match_templates_map
        };
        let all_template = template_map.get("All").unwrap();

        let result = all_template.consume_from_start("A ABB", false, &template_map).unwrap();
        assert_eq!(result.0, TokenParseStatus::FullParse); // status
        assert_eq!(result.1, 5); // offset
        assert_eq!(result.3.len(), 2); // child_ids
        assert_eq!(result.4.tokens.len(), 13); // tokens_collection
        assert_eq!(result.4.stringify(), "A ABB");
        // dump(result.3[0], &result.4.tokens);
    }
}
