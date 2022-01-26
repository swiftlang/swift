// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

// Note there is purposefully no trailing newline here.
// expected-error@+1 {{unterminated regex literal}}
var unterminated = '/xy