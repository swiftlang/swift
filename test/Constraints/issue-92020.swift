// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/92020
//
// In diagnostic mode, the missing `Sequence` conformance on the function type
// is fixed, and `((Int?) -> ()).Element` was left behind as a concrete
// dependent member type instead of a hole, because a function type cannot
// have members. It then ended up as an inferred collection element type and
// tripped an assertion in the binding inference.

func x(x: Int?) {}
_ = (x + x) + (x + x)
// expected-error@-1 {{binary operator '+' cannot be applied to two '(Int?) -> ()' operands}}

func y(y: String?) {}
_ = (y + y) + (y + y)
// expected-error@-1 {{binary operator '+' cannot be applied to two '(String?) -> ()' operands}}

// Metatypes cannot have members either. Make sure we only diagnose the
// conformance failure and not a bogus follow-on about `Int.Type.Element`.
func f<T: Sequence>(_: T) where T.Element == Int {}
// expected-note@-1 {{required by global function 'f' where 'T' = 'Int.Type'}}
f(Int.self)
// expected-error@-1 {{type 'Int.Type' cannot conform to 'Sequence'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

func g<T: Sequence>(_: T, _: (T.Element) -> Void) {}
// expected-note@-1 {{required by global function 'g' where 'T' = 'Int.Type'}}
g(Int.self) { (_: Int) in }
// expected-error@-1 {{type 'Int.Type' cannot conform to 'Sequence'}}
// expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
