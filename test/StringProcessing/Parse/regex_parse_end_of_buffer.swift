// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex
// REQUIRES: swift_swift_parser

// Note there is purposefully no trailing newline here.
// expected-cparser-error@+2:20 {{unterminated regex literal}} expected-astgen-error@+2:25 {{expected '/#' to end regex literal}}
// expected-error@+1:25 {{cannot parse regular expression: expected ')'}}
var unterminated = #/(xy