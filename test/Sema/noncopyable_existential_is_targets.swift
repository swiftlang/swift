// RUN: %target-typecheck-verify-swift -target %target-future-triple -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

// Target and source kinds for a type test involving noncopyable types.
//
// `as?`/`as!` remain rejected; see noncopyable_existential_casting.swift.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct Big: ~Copyable, P { var tag: Int }
struct Other: ~Copyable, Q { var tag: Int }
struct Both: ~Copyable, P, Q { var tag: Int }

// Covered as an execution test in
// test/Interpreter/noncopyable_is_relaxed_targets.swift.
func toOtherExistential(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is any Q & ~Copyable
}

// A `Copyable` existential target, likewise a runtime question.
func toCopyableExistential(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is any P
}

// A generic parameter.
func toGenericParameter<T: ~Copyable>(_ box: borrowing any P & ~Copyable,
                                      _: T.Type) -> Bool {
  return box is T
}

// A concrete noncopyable source, statically decidable.
func fromConcrete(_ x: borrowing Big) -> Bool {
  return x is any P & ~Copyable
  // expected-warning @-1 {{'is' test is always true}}
}

func fromConcreteToUnrelated(_ x: borrowing Big) -> Bool {
  return x is Other
  // expected-warning @-1 {{cast from 'Big' to unrelated type 'Other' always fails}}
}

// A generic noncopyable source.  Not statically decidable.
func fromGeneric<S: ~Copyable>(_ x: borrowing S) -> Bool {
  return x is Big
}

// `AnyObject` is reached by boxing in `__SwiftValue`, which copies, so a
// concrete non-`Copyable` value can never be one. A concrete source is therefore
// statically false...
func concreteToAnyObject(_ x: borrowing Big) -> Bool {
  return x is AnyObject
  // expected-warning @-1 {{cast from 'Big' to unrelated type 'AnyObject' always fails}}
}

// ... while an existential noncopyable may hold a `Copyable` payload
func existentialToAnyObject(_ box: borrowing any P & ~Copyable) -> Bool {
  return box is AnyObject
}

func caseIsOtherExistential(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is any Q & ~Copyable: return 1
  default: return -1
  }
}

// TODO: Metatype casts need further work.
func metatypeTest(_ t: Big.Type) -> Bool {
  return t is any P.Type
  // expected-warning @-1 {{cast from 'Big.Type' to unrelated type 'any P.Type' always fails}}
}
