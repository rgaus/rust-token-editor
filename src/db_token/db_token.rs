use std::error::Error;
use std::rc::Rc;
use std::collections::HashMap;
use std::fmt;

use crate::token::*;
use crate::token_match_template::*;
use crate::Tokenable;
use crate::DbTokenCollection;

#[derive(Debug)]
struct DbTokenReparseError {
    text: String,
}
impl DbTokenReparseError {
    fn new(text: String) -> Self {
        Self { text }
    }
}

impl fmt::Display for DbTokenReparseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Error for DbTokenReparseError {}

#[derive(Debug)]
#[derive(Clone)]
pub struct DbToken<'a> {
    token_collection: Rc<&'a DbTokenCollection>,

    pub id: i64,
    pub template: TokenMatchTemplateMatcher,
    pub literal: Option<String>,
    pub matches: HashMap<String, TokenMatch>,

    // pub events: TokenEvents,

    // pub effects: Vec<TokenEffect>,

    pub next_id: Option<i64>,
    pub previous_id: Option<i64>,
    pub parent_id: Option<i64>,
    pub children_ids: Vec<i64>,
}

impl<'a> DbToken<'a> {
    pub fn new(
        token_collection: Rc<&'a DbTokenCollection>,
        id: i64,
        literal: Option<String>,
        template: TokenMatchTemplateMatcher,
        matches: HashMap<String, TokenMatch>,
        next_id: Option<i64>,
        previous_id: Option<i64>,
        parent_id: Option<i64>,
        children_ids: Vec<i64>,
    ) -> Self {
        Self {
            token_collection,
            id,
            template,
            literal,
            matches,
            next_id,
            previous_id,
            parent_id,
            children_ids,
        }
    }
}

impl<'a> Tokenable<i64> for DbToken<'a> {
    fn next(&self) -> Result<Option<Box<Self>>, Box<dyn Error>> {
        match self.next_id {
            Some(next_id) => self.token_collection.get_by_id(next_id).map(|token| match token {
                Some(token) => Some(token),
                None => None,
            }),
            None => Ok(None),
        }
    }
    fn previous(&self) -> Result<Option<Box<Self>>, Box<dyn Error>> {
        match self.previous_id {
            Some(previous_id) => self.token_collection.get_by_id(previous_id).map(|token| match token {
                Some(token) => Some(token),
                None => None,
            }),
            None => Ok(None),
        }
    }
    fn parent(&self) -> Result<Option<Box<Self>>, Box<dyn Error>> {
        match self.parent_id {
            Some(parent_id) => self.token_collection.get_by_id(parent_id).map(|token| match token {
                Some(token) => Some(token),
                None => None,
            }),
            None => Ok(None),
        }
    }
    fn children(&self) -> Result<Vec<Box<Self>>, Box<dyn Error>> {
        let mut children = vec![];
        for child_id in &self.children_ids {
            if let Some(token) = self.token_collection.get_by_id(*child_id)? {
                children.push(token);
            }
        }
        Ok(children)
    }
    fn id(&self) -> i64 {
        self.id
    }
    fn literal(&self) -> &Option<String> {
        &self.literal
    }
}
