// RUN: %swift %s -parse -verify
// XFAIL: *
func incompleteTypeFunc(a, b) -> Int { return 1 } // expected-error {{type annotation missing in pattern}} expected-error{{type annotation missing in pattern}}
var incompleteTypeVar = incompleteTypeFunc
incompleteTypeVar(1, 1)
