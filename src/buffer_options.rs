use std::collections::HashMap;
use std::ops::Index;

fn coerse_to_bool(raw_string: &str) -> bool {
    raw_string.to_ascii_lowercase().starts_with("t")
}


// BufferOptions are used to track options that can be set or cleared which will effect how the
// editor works.
#[derive(Debug, Clone)]
pub struct BufferOptions {
    options: HashMap<String, String>,
}
impl BufferOptions {
    pub fn new_empty() -> Self {
        Self { options: HashMap::new() }
    }
    pub fn new_with_defaults() -> Self {
        let mut options = Self::new_empty();
        options.insert("matchpairs", "(:),{:},[:],<:>");
        // options.insert("cpoptions", "aABceFs");
        options.insert("cpoptions", "M");
        options
    }

    pub fn insert(&mut self, key: &str, value: &str) {
        self.options.insert(String::from(key), String::from(value));
    }
    pub fn append(&mut self, key: &str, value: &str) {
        self.options.insert(String::from(key), format!("{}{}", self.get(key), value));
    }
    pub fn clear(&mut self, key: &str) {
        self.options.remove(key);
    }

    // Used by the % / MatchDelimeters command to figure out which delimeters should match.
    pub fn supported_match_delimeters(&self) -> Vec<(&str, &str, Option<&str>)> {
        self.get("matchpairs").split(",").flat_map(|open_close_end| {
            match open_close_end.split(":").collect::<Vec<&str>>()[..] {
                [open, close] => vec![(open, close, None)],
                [open, close, end] => vec![(open, close, Some(end))],
                [..] => vec![],
            }
        }).collect()
    }

    // Used by the % / MatchDelimeters command to figure out if prefixed backslash escapes before
    // `supported_match_delimeters` should only ever match with other escaped delimeters
    pub fn should_match_escaped_delimeters_distinctly(&self) -> bool {
        self.get("cpoptions").contains("M")
    }

    pub fn sentence_must_be_followed_by_double_space_no_tab(&self) -> bool {
        self.get("cpoptions").contains("J")
    }

    pub fn get(&self, key: &str) -> &str {
        match self.options.get(key) {
            Some(value) => value,
            None => "",
        }
    }
}
impl Index<&str> for BufferOptions {
    type Output = str;

    fn index(&self, key: &str) -> &str {
        self.get(key)
    }
}
