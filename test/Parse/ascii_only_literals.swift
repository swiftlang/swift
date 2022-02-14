// RUN: %target-typecheck-verify-swift -string-literals-must-be-ascii-only

_ = "Normal string"
// ok

_ = "String with a café"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = "String with a 🍿"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = "String with a \u{24}"
// ok - U+0024 is just an ASCII dollar sign

_ = "String with some control characters \u{1} \u{2} \u{3} \u{4}"
// ok

_ = "String with a \u{2665}"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = "String with a non-breaking space"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = "String with another non-breaking space"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = """
    multi
    line
    ascii
    string
    """
// ok

_ = """
    multi
    line
    string
    with 🍿
    """
// expected-error@-6{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}

_ = "String with a null\u{0}byte"
// ok - NUL is allowed

_ = "String with a null\u{0}byte and a \u{2665}"
// expected-error@-1{{non-ASCII character found when compiling with -string-literals-must-be-ascii-only}}
