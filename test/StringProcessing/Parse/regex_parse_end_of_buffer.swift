// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_swift_parser

// Note there is purposefully no trailing newline here.
// expected-error@+2:20 {{unterminated regex literal}}
// expected-error@+1:25 {{cannot parse regular expression: expected ')'}}
var unterminated = #/(xy