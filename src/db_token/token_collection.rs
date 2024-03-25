use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;

use rusqlite::{Connection, Result};

use crate::token::*;
use crate::token_match_template::*;
use crate::DbToken;


/// The DbTokenCollection is powered by rusqlite, which returns `Result<T, rusqlite::Error>`s all
/// over the place. However, rusqlite::Error does not implement the std::error:Error trait, which
/// means that when these errors bubble up to higher calling contexts, it's impossible to treat
/// them as part of that common trait that other errors fall under.
///
/// To work around this, `RusqliteRealError` is a wrapper type that implements `Error`. Any
/// time a rusqlite::Error is returned, it is first converted into an instance of this
/// RusqliteRealError first so that it implements the trait.
#[derive(Debug)]
struct RusqliteRealError {
    error: rusqlite::Error,
}
impl fmt::Display for RusqliteRealError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.error)
    }
}
impl Error for RusqliteRealError {}
impl RusqliteRealError {
    fn new_from_rusqlite(error: rusqlite::Error) -> Self {
        RusqliteRealError { error }
    }
}


#[derive(Debug)]
pub struct DbTokenCollection {
    connection: Connection,
    token_match_templates_map: Rc<TokenMatchTemplateMap>,
}

impl DbTokenCollection {
    pub fn new(
        generated_tokens: Vec<Box<Token>>,
        token_match_templates_map: Rc<TokenMatchTemplateMap>,
    ) -> Result<Self, Box<dyn Error>> {
        let token_collection = Self {
            connection: Connection::open_in_memory()?,
            // connection: Connection::open("./db.sqlite3")?,
            token_match_templates_map,
        };

        token_collection.reset_and_migrate()?;

        token_collection.extend_from_generated_tokens(generated_tokens)?;

        Ok(token_collection)
    }

    fn reset_and_migrate(&self) -> Result<(), Box<dyn Error>> {
        self.connection.execute("
            CREATE TABLE tokens (
                id           INTEGER PRIMARY KEY,
                next_id      INTEGER,
                previous_id  INTEGER,
                parent_id    INTEGER,
                /* children_ids BLOB, */

                literal      TEXT,

                template     BLOB NOT NULL,
                matches      BLOB NOT NULL,
                events       BLOB NOT NULL,
                effects      BLOB NOT NULL
            );
        ", ()).or_else(|err| Err(Box::new(RusqliteRealError::new_from_rusqlite(err)) as Box<dyn Error>))?;

        Ok(())
    }


    pub fn new_empty() -> Result<Self, Box<dyn Error>> {
        Self::new(vec![], Rc::new(HashMap::new()))
    }

    pub fn new_unparsed_literal(literal: &str) -> Result<Self, Box<dyn Error>> {
        let token_collection = Self::new_empty()?;
        token_collection.push_raw(
            &Some(String::from(literal)),
            None, // next_id
            None, // previous_id
            None, // parent_id
        )?;

        Ok(token_collection)
    }

    // Given a vec of regular `Token`s, convert them into the proper format so they can be inserted
    // into the database
    fn extend_from_generated_tokens(&self, tokens: Vec<Box<Token>>) -> Result<(), Box<dyn Error>> {
        let mut uuid_to_id: HashMap<uuid::Uuid, i64> = HashMap::new();
        let mut id_to_next_id: HashMap<i64, uuid::Uuid> = HashMap::new();

        // Find root token
        let root_token = tokens.iter().find(|token| token.previous_id.is_none());

        let mut cursor = root_token;
        loop {
            match cursor {
                Some(cursor_token) => {
                    let converted_previous_id = cursor_token.previous_id.map(|uuid| uuid_to_id.get(&uuid).expect("Cannot find previous_id in uuid_to_id!"));
                    let converted_parent_id = cursor_token.parent_id.map(|uuid| uuid_to_id.get(&uuid).expect("Cannot find parent_id in uuid_to_id!"));

                    let id = self.push_raw(
                        &cursor_token.literal,
                        None,
                        converted_previous_id,
                        converted_parent_id,
                    )?;
                    uuid_to_id.insert(cursor_token.id, id);
                    if let Some(next_id) = cursor_token.next_id {
                        id_to_next_id.insert(id, next_id);
                    }

                    // Move to the next token
                    cursor = tokens.iter().find(|t| Some(t.id) == cursor_token.next_id);
                },
                None => { break; },
            }
        }

        for (id, old_next_id) in id_to_next_id {
            if let Some(next_id) = uuid_to_id.get(&old_next_id) {
                self.connection.execute(
                    "update tokens set next_id=?1 where id=?2",
                    (next_id, id),
                ).or_else(|err| Err(Box::new(RusqliteRealError::new_from_rusqlite(err)) as Box<dyn Error>))?;
            }
        }

        Ok(())
    }

    // pub fn push(&self, token: Box<Token>) -> Result<(), Box<dyn Error>> {
    //     self.connection.execute(
    //         "INSERT INTO token (next_id, previous_id, parent_id, literal) VALUES (?1, ?2, ?3, ?4)",
    //         (&token.next_id, &token.previous_id, &token.parent_id, &token.literal),
    //     ).or_else(|err| Err(Box::new(RusqliteRealError::new_from_rusqlite(err)) as Box<dyn Error>))?;
    // }

    pub fn push_raw(
        &self,
        literal: &Option<String>,
        next_id: Option<&i64>,
        previous_id: Option<&i64>,
        parent_id: Option<&i64>,
    ) -> Result<i64, Box<dyn Error>> {
        self.connection.execute(
            "insert into tokens (next_id, previous_id, parent_id, literal, template, matches, events, effects) values (?2, ?2, ?3, ?4, \"\", \"\", \"\", \"\")",
            (&next_id, &previous_id, &parent_id, &literal),
        ).or_else(|err| Err(Box::new(RusqliteRealError::new_from_rusqlite(err)) as Box<dyn Error>))?;

        Ok(self.connection.last_insert_rowid())
    }

    // For debugging, dump out the whole table
    pub fn dump(&self) -> Result<Vec<DbToken>, Box<dyn Error>> {
        Ok(self.connection
            .prepare("select id, literal, next_id, previous_id, parent_id from tokens")?
            .query_map([], |row| {
                Ok(DbToken::new(
                    Rc::new(self),

                    row.get(0)?, // id
                    row.get(1)?, // literal
                    HashMap::new(), // matches
                    row.get(2)?, // next_id
                    row.get(3)?, // previous_id
                    row.get(4)?, // parent_id
                    vec![], // children_ids

                    // effects: vec![],
                    // events: vec![],
                ))
            })?
            .map(|wrapped_token| wrapped_token.unwrap())
            .collect::<Vec<DbToken>>())
    }

    pub fn get_by_id<'a>(&'a self, id: i64) -> Result<Option<Box<DbToken>>, Box<dyn Error>> {
        match self.connection.query_row(
            "select id, literal, next_id, previous_id, parent_id from tokens where id = ?1",
            [id],
            |row| {
                Ok(DbToken::new(
                    Rc::new(self),

                    row.get(0)?, // id
                    row.get(1)?, // literal
                    HashMap::new(), // matches
                    row.get(2)?, // next_id
                    row.get(3)?, // previous_id
                    row.get(4)?, // parent_id
                    vec![], // children_ids

                    // effects: vec![],
                    // events: vec![],
                ))
            },
        ) {
            Ok(dbtoken) => Ok(Some(Box::new(dbtoken))),
            Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
            Err(err) => Err(Box::new(RusqliteRealError::new_from_rusqlite(err))),
        }
    }

    // TODO: get_by_offset
    // TODO: compute_offset
}
