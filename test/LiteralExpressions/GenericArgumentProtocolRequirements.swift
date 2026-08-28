// Parenthesized generic arguments in a protocol requirement.
//
// Parentheses route a generic argument onto the expression path, where
// resolving a name needs unqualified value lookup. Inside a protocol that
// lookup needs the requirement signature, which is what computing a structural
// requirement asks for, so the request used to cycle.
//
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions

protocol Container<T> { associatedtype T }

// A bare argument parses as a type and never reaches the expression path.
protocol BareName {
  associatedtype A
  associatedtype B: Container<A>
}

// A single parenthesized name is indistinguishable from a generic value such
// as '(N)', so the parser leaves it on the expression path.
protocol ParenName {
  associatedtype A
  associatedtype B: Container<(A)>
}

// A top-level comma makes the argument a tuple type.
protocol ParenTuple {
  associatedtype A
  associatedtype B: Container<(A, Self)>
}

// Nested parentheses put the comma below the top level.
protocol NestedParenTuple {
  associatedtype A
  associatedtype B: Container<((A, Self))>
}

// 'Self' as a parenthesized argument.
protocol ParenSelf {
  associatedtype B: Container<(Self)>
}

// A generic value argument still resolves as a value, in and out of
// parentheses.
struct Vec<let N: Int, T> {}

struct ValueArguments<let N: Int> {
  typealias Bare = Vec<N, Int>
  typealias Paren = Vec<(N), Int>
  typealias Literal = Vec<(3), Int>
  typealias Folded = Vec<(2 + 3), Int>
}

// A value member with a concrete root stays on the value path.
struct Limits { static let count = 4 }

struct ValueMemberArgument {
  typealias A = Vec<(Limits.count), Int>
}

// A parenthesized value argument in a same-type requirement still forms the
// requirement, so a mismatched witness is still diagnosed.
protocol SameTypeValue {
  associatedtype X where X == Vec<(3), Int>
}

struct MismatchedWitness: SameTypeValue { // expected-error {{type 'MismatchedWitness' does not conform to protocol 'SameTypeValue'}}
  // expected-error@-1 {{'SameTypeValue' requires the types 'MismatchedWitness.X' (aka 'Vec<4, Int>') and 'Vec<3, Int>' be equivalent}}
  // expected-note@-2 {{requirement specified as 'Self.X' == 'Vec<3, Int>' [with Self = MismatchedWitness]}}
  typealias X = Vec<4, Int>
}
