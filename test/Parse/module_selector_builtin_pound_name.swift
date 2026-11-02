// RUN: %target-typecheck-verify-swift

// A module selector makes '#Module::name' an ordinary macro expansion, even
// when 'name' is the same as a builtin pound keyword.

func f(x: Int = #Swift::sourceLocation) {}
// expected-error@-1 {{no macro named 'Swift::sourceLocation'}}

// The stdlib macro '#file' is found through the selector.
func g(x: String = #Swift::file) {}
