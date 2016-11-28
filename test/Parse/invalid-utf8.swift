// RUN: %target-typecheck-verify-swift

var êx = "" // expected-error{{invalid UTF-8 found in source file}} {{5-6= }} 

// Make sure we don't stop processing the whole file.
static func foo() {} // expected-error{{static methods may only be declared on a type}} {{1-8=}}
