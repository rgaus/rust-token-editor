use std::collections::HashMap;

pub struct TokensCollection {
    pub tokens: Vec<Box<Token>>,
    pub offset_cache: HashMap<uuid::Uuid, usize>,
}

impl TokensCollection {
    pub fn new(tokens: Vec<Box<Token>>) -> TokensCollection {
        TokensCollection { tokens: tokens, offset_cache: HashMap::new() }
    }
    pub fn new_empty() -> TokensCollection {
        TokensCollection::new(vec![])
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
            for mut token in &self.tokens {
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
            for mut token in &self.tokens {
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

    // pub fn compute_offset<'a>(
    //     &'a mut self,
    //     id: uuid::Uuid,
    // ) -> usize {
    //     0
    //     // if let Some(cached_offset) = self.offset_cache.get(&self.id) {
    //     //     return *cached_offset;
    //     // }
    //     //
    //     // let Some(token) = tokens_collection.get_by_id(id) else {
    //     //     return 0;
    //     // };
    //     // let Some(previous_id) = self.previous_id else {
    //     //     return 0;
    //     // };
    //     // let Some(previous) = tokens_collection.get_by_id(previous_id) else {
    //     //     return 0;
    //     // };
    //     //
    //     // let previous_length = match &previous.literal {
    //     //     Some(literal) => literal.len(),
    //     //     None => 0,
    //     // };
    //     //
    //     // // The current offset is equal to the previous offset plus the length of the previous token
    //     // let previous_offset = {
    //     //     previous.compute_offset(tokens_collection)
    //     // };
    //     // let offset = previous_offset + previous_length;
    //     //
    //     // tokens_collection.offset_cache.insert(self.id, offset);
    //     // offset
    // }
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
    pub fn previous<'a>(&'a self, tokens_collection: &'a mut TokensCollection) -> Option<&Box<Token>> {
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

    // pub fn compute_offset<'a>(&'a self, tokens_collection: &'a mut TokensCollection) -> usize {
    //     tokens_collection.compute_offset(self.id)
    // }
}
