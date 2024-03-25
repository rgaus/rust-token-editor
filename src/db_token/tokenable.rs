use std::error::Error;
use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;

use rusqlite::{Connection, Result};

use crate::token::*;
use crate::token_match_template::*;

pub trait Tokenable: Clone {
    fn next(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn previous(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn parent(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn children(&self) -> Result<Vec<Box<Self>>, Box<dyn Error>>;

    fn depth(&self) -> Result<usize, Box<dyn Error>> {
        let mut pointer: Box<Self> = Box::new(self.clone());
        let mut depth = 0;
        loop {
            let Some(parent) = pointer.parent()? else {
                return Ok(depth);
            };
            let cloned = parent.clone();
            *pointer = *cloned;
            depth += 1;
        }
    }

    // fn compute_offset(&self) -> usize {
    //     self.token_collection.compute_offset(self.id)
    // }

    /// When called on a token, finds a direct child that matches the given predicate function.
    fn find_child<'a, F>(
        &'a self,
        mut matcher: F
    ) -> Result<Option<Box<Self>>, Box<dyn Error>> where F: FnMut(&Self) -> bool {
        for child in self.children()? {
            if matcher(&child) {
                return Ok(Some(child));
            };
        };
        Ok(None)
    }

    fn find_children<F>(&self, mut matcher: F) -> Result<Vec<Box<Self>>, Box<dyn Error>> where F: FnMut(&Self) -> bool {
        let mut matches = vec![];
        for child in self.children()? {
            if matcher(&child) {
                matches.push(child);
            };
        };
        Ok(matches)
    }
    fn deep_children(&self, max_depth: Option<usize>) -> Result<Vec<Box<Self>>, Box<dyn Error>> {
        let mut children = vec![];
        for child in self.children()? {
            children.push(child.clone());

            if max_depth == Some(0) {
                continue;
            };

            let next_max_depth = if let Some(max_depth) = max_depth {
                Some(max_depth - 1)
            } else {
                None
            };
            let deep_children = child.deep_children(next_max_depth)?;
            if deep_children.len() == 0 {
                continue;
            }
            children.extend(deep_children);
        }
        Ok(children)
    }
    fn find_deep_child<'a, F>(
        &'a self,
        max_depth: Option<usize>,
        mut matcher: F
    ) -> Result<Option<Box<Self>>, Box<dyn Error>> where F: FnMut(&Self) -> bool {
        let children = self.deep_children(max_depth)?;

        for child in children {
            if matcher(&child) {
                return Ok(Some(child));
            };
        };
        Ok(None)
    }
    fn find_deep_children<'a, F>(
        &'a self,
        max_depth: Option<usize>,
        mut matcher: F
    ) -> Result<Vec<Box<Self>>, Box<dyn Error>> where F: FnMut(&Self) -> bool {
        let children = self.deep_children(max_depth)?;

        let mut matches = vec![];
        for child in children {
            if matcher(&child) {
                matches.push(child);
            };
        };
        Ok(matches)
    }
}
