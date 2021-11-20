// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: libswift

var s = 'abc'
// expected-note@-1{{'Hello, 'abc''}}
// expected-error@-2{{single-quoted string literal found, use '"'}}
