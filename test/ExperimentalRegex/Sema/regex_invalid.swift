// RUN: %target-typecheck-verify-swift -enable-experimental-string-processing
// REQUIRES: libswift

'abc' // expected-error {{cannot find 'Regex' type in scope}}
