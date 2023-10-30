use std::collections::HashMap;
use std::ops::Index;

fn coerse_to_bool(raw_string: &str) -> bool {
    raw_string.to_ascii_lowercase().starts_with("t")
}

fn coerse_to_usize(raw_string: &str) -> usize {
    raw_string.parse::<usize>().unwrap_or(0)
}

fn string_of_n_spaces(n: usize) -> String {
    (0..n).map(|_| " ").collect::<String>()
}

fn unabbreviate(option_name: &str) -> &str {
    match option_name {
        "cop" => "cpoptions",
        "ai" => "autoindent",
        "sw" => "shiftwidth",
        "ts" => "tabstop",
        "et" => "expandtab",
        other => other,
    }
}
fn unabbreviate_and_unprefix(option_name: &str) -> (&str, bool) {
    if option_name.starts_with("no") {
        (unabbreviate(&option_name[2..]), true)
    } else {
        (unabbreviate(option_name), false)
    }
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
        options.insert("cpoptions", "MH");

        options.insert("autoindent", "true");
        options.insert("smartindent", "true");
        options.insert("cinwords", "if,else,while,do,for,switch"); // "c indent words"

        options.insert("shiftwidth", /* "8" */ "2"); // Pressing tab inserts this many spaces
        options.insert("tabstop", /* "8" */ "2"); // One tab is this many spaces
        options.insert("expandtab", "true"); // expandtab means turn tabs into spaces
        options.insert("softtabstop", "0");

        options
    }

    pub fn insert(&mut self, key: &str, value: &str) {
        let (long_key, is_inverted) = unabbreviate_and_unprefix(key);
        let processed_value = if value.is_empty() {
            if is_inverted { String::from("false") } else { String::from("true") }
        } else {
            String::from(value)
        };
        self.options.insert(String::from(long_key), processed_value);
    }
    pub fn clear(&mut self, key: &str) {
        let (long_key, _) = unabbreviate_and_unprefix(key);
        self.options.remove(long_key);
    }

    // For dealing with booleans:
    // options.set("foo") / options.set("nofoo")
    pub fn set(&mut self, key: &str) {
        self.insert(key, "")
    }

    // For dealing with lists of option fields:
    pub fn append(&mut self, key: &str, value: &str) {
        self.options.insert(String::from(unabbreviate_and_unprefix(key).0), format!("{}{}", self.get(key), value));
    }
    pub fn remove(&mut self, key: &str, option: &str) {
        self.options.insert(
            String::from(unabbreviate_and_unprefix(key).0),
            self.get(key).replace(option, "")
        );
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

    pub fn uppercase_i_skips_leading_space(&self) -> bool {
        self.get("cpoptions").contains("H")
    }

    pub fn autoindent_when_creating_new_lines(&self) -> bool {
        coerse_to_bool(self.get("autoindent"))
    }
    pub fn smartindent_enabled(&self) -> bool {
        coerse_to_bool(self.get("smartindent"))
    }

    // When a user presses enter at the end of a line line `if foo {`, what should be inserted to
    // go to the next indentation level?
    pub fn get_smartindent_indentation_level(&self) -> String {
        match coerse_to_bool(self.get("expandtab")) {
            true => {
                let shiftwidth = coerse_to_usize(self.get("shiftwidth"));
                if shiftwidth == 0 {
                    let tabstop = coerse_to_usize(self.get("tabstop"));
                    string_of_n_spaces(tabstop)
                } else {
                    string_of_n_spaces(shiftwidth)
                }
            },
            false => String::from("\t"),
        }
    }
    // Words that if a line starts with, will trigger smartindenting behavior
    pub fn get_smartindent_prefix_words(&self) -> Vec<&str> {
        self.get("cinwords").split(",").collect()
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
