// RUN: %swift %s -parse -verify

var êx = "" // expected-error{{invalid UTF-8 found in source file}} expected-error{{expected pattern}}

// Make sure we don't stop processing the whole file.
static func foo() {} // expected-error{{static functions may only be declared on a type}}
