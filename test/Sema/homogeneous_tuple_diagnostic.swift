// RUN: %target-typecheck-verify-swift

// Homogeneous tuples with 5+ elements should print in compact form.
func largeTuple(_: (Int, Int, Int, Int, Int)) {}
largeTuple("hello") // expected-error {{cannot convert value of type 'String' to expected argument type '(Int /* ... repeated 5 times ... */)'}}

// Homogeneous tuples with 4 or fewer elements should print normally.
func smallTuple(_: (Int, Int, Int, Int)) {}
smallTuple("hello") // expected-error {{cannot convert value of type 'String' to expected argument type '(Int, Int, Int, Int)'}}

// Labeled tuples should not be coalesced.
func labeledTuple(_: (a: Int, b: Int, c: Int, d: Int, e: Int)) {}
labeledTuple("hello") // expected-error {{cannot convert value of type 'String' to expected argument type '(a: Int, b: Int, c: Int, d: Int, e: Int)'}}

// Heterogeneous tuples should not be coalesced.
func heterogeneousTuple(_: (Int, Int, Int, Int, Bool)) {}
heterogeneousTuple("hello") // expected-error {{cannot convert value of type 'String' to expected argument type '(Int, Int, Int, Int, Bool)'}}