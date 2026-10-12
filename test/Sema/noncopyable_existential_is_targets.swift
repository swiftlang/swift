// RUN: %target-typecheck-verify-swift -target %target-future-triple -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// Target kinds that Sema still rejects for a conditional cast out of a
// noncopyable existential. These predate the non-consuming type test and are
// not properties of it -- the test itself has no trouble with either target.
// They live in their own file because a Sema-phase error aborts before SILGen,
// which would suppress the diagnostics in
// test/SILOptimizer/moveonly_noncopyable_existential_is.swift.
//
// Recorded so that lifting either restriction is a deliberate change with a
// test to update.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Big: ~Copyable, P { var tag: Int }

func toOtherExistential(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is any Q & ~Copyable
  // expected-error @-1 {{noncopyable types cannot be conditionally cast}}
  // expected-warning @-2 {{cast from 'any P & ~Copyable' to unrelated type 'any Q & ~Copyable' always fails}}
}

func toGenericParameter<T: ~Copyable>(_ box: borrowing any P & ~Copyable,
                                      _: T.Type) -> Bool {
  return box is T
  // expected-error @-1 {{noncopyable types cannot be conditionally cast}}
  // expected-warning @-2 {{cast from 'any P & ~Copyable' to unrelated type 'T' always fails}}
}

// Note the asymmetry: the same target *is* accepted in pattern position, where
// it lowers to a non-consuming type test like any other. Only the expression
// form is rejected. Covered as a positive case in
// test/Interpreter/noncopyable_existential_is.swift.
func caseIsOtherExistential(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is any Q & ~Copyable: return 1
  // expected-warning @-1 {{cast from 'any P & ~Copyable' to unrelated type 'any Q & ~Copyable' always fails}}
  default: return -1
  }
}
