# RegexBuilder

Use an expressive domain-specific language to build regular expressions,
for operations like searching and replacing in text.

## Overview

Regular expressions, also known as regexes,
are a powerful tool for matching patterns in text.
Swift supports several ways to create a regular expression,
including from a string, as a literal, and using this DSL.
For example:

```swift
let word = OneOrMore(.word)
let emailPattern = Regex {
    Capture {
        ZeroOrMore {
            word
            "."
        }
        word
    }
    "@"
    Capture {
        word
        OneOrMore {
            "."
            word
        }
    }
}

let text = "My email is my.name@example.com."
if let match = text.firstMatch(of: emailPattern) {
    let (wholeMatch, name, domain) = match.output
    // wholeMatch is "my.name@example.com"
    // name is "my.name"
    // domain is "example.com"
}
```

## Topics

### Components

- ``Swift/RegexComponent``
- ``RegexBuilder/CharacterClass``
- ``RegexBuilder/Anchor``
- ``RegexBuilder/Lookahead``
- ``RegexBuilder/NegativeLookahead``
- ``RegexBuilder/ChoiceOf``

### Quantifiers

- ``RegexBuilder/One``
- ``RegexBuilder/Optionally``
- ``RegexBuilder/ZeroOrMore``
- ``RegexBuilder/OneOrMore``
- ``RegexBuilder/Repeat``
- ``RegexBuilder/Local``

### Captures

- ``RegexBuilder/Capture``
- ``RegexBuilder/TryCapture``
- ``RegexBuilder/Reference``

### Builders

- ``RegexBuilder/RegexComponentBuilder``
- ``RegexBuilder/AlternationBuilder``
