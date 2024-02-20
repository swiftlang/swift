// RUN: %target-typecheck-verify-swift

protocol P<A> {
  associatedtype A
}

func f<each T>(_: some P<repeat each T>) {}
// expected-error@-1 {{pack expansion 'repeat each T' can only appear in a function parameter list, tuple element, or generic argument of a variadic type}}
// expected-error@-2 {{generic parameter 'T' is not used in function signature}}