// RUN: %target-typecheck-verify-swift -enable-bare-slash-regex -disable-availability-checking
// REQUIRES: swift_in_compiler

// Note there is purposefully no trailing newline here.
// expected-error@+1 {{unterminated regex literal}}
var unterminated = #/xy
