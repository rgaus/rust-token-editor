use std::error::Error;
use std::cmp::PartialEq;
use rusqlite::Result;

pub trait Tokenable<Id: PartialEq>: Clone {
    fn next(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn previous(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn parent(&self) -> Result<Option<Box<Self>>, Box<dyn Error>>;
    fn children(&self) -> Result<Vec<Box<Self>>, Box<dyn Error>>;

    fn id(&self) -> Id;
    fn literal(&self) -> &Option<String>;

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

    // When called, gets the literal text of this token and all of its decendants.
    fn stringify(&self) -> Result<String, Box<dyn Error>> {
        let mut result = String::from("");
        let mut pointer: Box<Self> = Box::new(self.clone());
        loop {
            if let Some(literal_text) = pointer.literal() {
                result = format!("{}{}", result, literal_text);
            };
            if let Some(next_pointer) = pointer.next()? {
                // Only keep going if the next token is a CHILD of the token being stringified!
                if self.find_deep_child(None, |child| child.id() == next_pointer.id())?.is_none() {
                    break;
                }

                pointer = next_pointer;
            } else {
                break;
            }
        }

        Ok(result)
    }

    /// Computes the "deep last child" of a token. This is defined as the last child's last child's
    /// last child (and so on). In diagram form:
    ///.    self
    ///.     / \
    ///.    a   b
    ///.   /\   /\
    ///.  c  d e  f <-- `f` is the "deep last child"
    ///
    /// If the specified token doesn't have any children, this function returns `None`.
    fn deep_last_child(&self) -> Result<Option<Box<Self>>, Box<dyn Error>> {
        let children = self.children()?;
        let mut last_child_token = if let Some(last_child_token) = children.last() {
            Some(last_child_token.clone())
        } else {
            None
        };

        loop {
            let Some(last_child_token_unwrapped) = last_child_token else {
                break;
            };
            last_child_token = Some(last_child_token_unwrapped.clone());
        }

        Ok(last_child_token)
    }
    fn deep_last_child_id(&self) -> Result<Option<Id>, Box<dyn Error>> {
        if let Some(child) = self.deep_last_child()? {
            Ok(Some(child.id()))
        } else {
            Ok(None)
        }
    }
}
