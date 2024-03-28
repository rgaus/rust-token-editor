use serde::{de, Serialize, Serializer, Deserialize, Deserializer};
use std::borrow::Cow;

// from: https://github.com/tailhook/serde-regex/blob/master/src/lib.rs

/// A wrapper type which implements `Serialize` and `Deserialize` for
/// types involving `Regex`
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Serde<T>(pub T);

impl Serialize for Serde<regex::Regex> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Serde<regex::Regex> {
    fn deserialize<D>(d: D) -> Result<Serde<regex::Regex>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = <Cow<str>>::deserialize(d)?;

        match s.parse() {
            Ok(regex) => Ok(Serde(regex)),
            Err(err) => Err(de::Error::custom(err)),
        }
    }
}
