pub const NEWLINE_CHAR: char = '\n';
pub const BACKSPACE_CHAR: char = '\x08';
pub const ESCAPE_CHAR: char = '\x1B';

pub const ESCAPE: &'static str = "\x1B";
pub const BACKSPACE: &'static str = "\x08";

pub fn is_word_char(c: char) -> bool {
    c.is_ascii_lowercase() || c.is_ascii_uppercase()
}
pub fn is_whitespace_char(c: char) -> bool {
    c.is_ascii_whitespace()
}
pub fn is_other_char(c: char) -> bool {
    !is_word_char(c) && !is_whitespace_char(c)
}
