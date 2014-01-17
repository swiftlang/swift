// RUN: %swift %s -parse -verify

var êx = "" // expected-error{{invalid UTF-8 found in source file}} expected-error{{expected pattern}}

// Make sure we don't stop processing the whole file.
type func foo() {} // expected-error{{type functions may only be declared on a type}}
