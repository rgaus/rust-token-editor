use std::collections::HashMap;
use regex::Regex;
use uuid::Uuid;

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

// fn stringify(
//     head_id: uuid::Uuid,
//     tokens: &mut Vec<Box<Token>>,
//     token_match_templates_map: &HashMap<&str, TokenMatchTemplate>,
// ) -> String {
//     let mut result = String::from("");
//     let mut pointer_id = head_id;
//     loop {
//         let Some(mut pointer) = tokens.iter().find(|t| t.id == pointer_id) else {
//             println!("BAD POINTER! {:?}", pointer_id);
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

    let input = "{let a = ['aaa', ['cc', 'bbb']]}";
    // let input = "456";

    // let input = "1aa1bb";

    match all_template.consume_from_start(input, &token_match_templates_map) {
        Ok((_matched_all, offset, _last_token_id, child_ids, mut tokens_collection)) => {
            // println!("RESULT: {:?} {:?}", offset, tokens_collection);
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
                    println!("{}", tokens_collection.stringify());
                }

                println!("RESULT: {:?}", tokens_collection.get_by_offset(13));

                println!("=========");
                println!("= MUTATION:");
                println!("=========");

                let token_id = {
                    let top = tokens_collection.get_by_id(child_ids[0]).unwrap();
                    let token = top.find_deep_child(&tokens_collection, 100, |token| match token {
                        Token { literal: Some(text), .. } if text == "'aaa'" => true,
                        _ => false,
                    }).unwrap();
                    token.id
                };

                let new_subtree_token_id = tokens_collection.change_token_literal_text(
                    token_id,
                    "aba".to_string(),
                    &token_match_templates_map,
                ).unwrap();

                println!("--------- {}", new_subtree_token_id);
                // dump(new_subtree_token_id, &tokens_collection.tokens);
                for child_id in &child_ids {
                    dump(*child_id, &tokens_collection.tokens);
                    println!("---------");
                }
                {
                    println!("{}", tokens_collection.stringify_to_end(child_ids[0]));
                }
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

    #[test]
    fn it_works() {
        let a = TokensCollection::new_empty();
        let result = 2 + 2;
        assert_eq!(result, 4);
    }


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

        #[test]
        fn it_fully_parses_1() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 1); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 3); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_12() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("12", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 2); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 6); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_112() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("112", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 3); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 9); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_1112() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1112", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 4); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 12); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_starts_to_parse_1112aa() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1112aa", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 4); // offset - NOTE: not the whole string!
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 12); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_parses_11112_as_just_1() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("11112", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 1); // offset - NOTE: not the whole string!
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 3); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_doesnt_parse_333() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("333", &template_map).unwrap();
            assert_eq!(result.0, false); // matched_all
            assert_eq!(result.1, 0); // offset - NOTE: not the whole string!
            assert_eq!(result.3.len(), 0); // child_ids
            assert_eq!(result.4.tokens.len(), 0); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_doesnt_parse_empty_string() {
            let template_map = initialize_mini_language_twelve();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("", &template_map).unwrap();
            assert_eq!(result.0, false); // matched_all
            assert_eq!(result.1, 0); // offset - NOTE: not the whole string!
            assert_eq!(result.3.len(), 0); // child_ids
            assert_eq!(result.4.tokens.len(), 0); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
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
                    Regex::new(r"^(?<value>-?[0-9])").unwrap(),
                ),
            ]));

            token_match_templates_map
        }

        #[test]
        fn it_fully_parses_1_plus_1() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+1", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 3); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 11); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_1_plus_negative_1() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+-1", &template_map).unwrap();
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 4); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 11); // tokens_collection
            // dump(result.3[0], &result.4.tokens);
        }

        #[test]
        fn it_fully_parses_1_plus_quantity_5_times_6() {
            let template_map = initialize_mini_language_math();
            let all_template = template_map.get("All").unwrap();

            let result = all_template.consume_from_start("1+(5*6)", &template_map).unwrap();
            dump(result.3[0], &result.4.tokens);
            assert_eq!(result.0, true); // matched_all
            assert_eq!(result.1, 7); // offset
            assert_eq!(result.3.len(), 1); // child_ids
            assert_eq!(result.4.tokens.len(), 23); // tokens_collection
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

        let result = all_template.consume_from_start("A ABB", &template_map).unwrap();
        assert_eq!(result.0, true); // matched_all
        assert_eq!(result.1, 5); // offset
        assert_eq!(result.3.len(), 2); // child_ids
        assert_eq!(result.4.tokens.len(), 13); // tokens_collection
        // dump(result.3[0], &result.4.tokens);
    }
}
