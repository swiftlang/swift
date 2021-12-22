// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: swift_in_compiler

let s = '/\\/''/ // expected-error {{unterminated regex literal}}

// expected-error@+1 {{unterminated regex literal}}
var unterminated = '/xy
