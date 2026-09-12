// Parenthesized generic arguments in a protocol requirement.
//
// Parentheses route a generic argument onto the expression path, where
// resolving a name needs unqualified value lookup. Inside a protocol that
// lookup needs the requirement signature, which is what computing a structural
// requirement asks for, so the request used to cycle.
//
// Only a protocol prefers a type interpretation of a name. Every other context
// keeps the ordinary value path, so the structs below pin that path against
// changing.
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

// A parenthesized dependent member type. Checking that 'Inner' names a type
// needs the base's conformances, which the requirement signature under
// computation would supply, so the member is folded from its syntax and
// checked later.
protocol Base { associatedtype Inner: Deep }
protocol Deep { associatedtype Deeper }

protocol ParenDependentMember {
  associatedtype A: Base
  associatedtype B: Container<(A.Inner)>
}

// The same, rooted at 'Self'.
protocol ParenSelfMember: Base {
  associatedtype B: Container<(Self.Inner)>
}

// A chain of more than one member.
protocol ParenDependentMemberChain {
  associatedtype A: Base
  associatedtype B: Container<(A.Inner.Deeper)>
}

// A concrete root still resolves through the general fold.
protocol ParenConcreteMember {
  associatedtype B: Container<(Int.Magnitude)>
}

// Metatype sugar is not a member type. A parenthesized 'A.Type' must still fold
// to a metatype, inside a protocol and outside one.
protocol ParenMetatype {
  associatedtype A
  associatedtype B: Container<(A.Type)>
}

struct Box<T> {}
struct MetatypeInTypealias<A> { typealias Boxed = Box<(A.Type)> }
struct MetatypeInProperty<A> { var x: Box<(A.Type)>? }

// An associated type is never itself a protocol, so 'A.Protocol' is an error.
// The error must be the accurate one, not 'Protocol is not a member type'.
protocol Marker {}
protocol ParenProtocolMetatype {
  associatedtype A: Marker
  associatedtype B: Container<(A.Protocol)>
  // expected-error@-1 {{cannot use 'Protocol' with non-protocol type 'Self.A'}}
}

// A value generic parameter in a struct never reaches the type-preferring
// lookup. Both errors below come from the value path, and folding a generic
// argument must not change them.
struct ValueParamMember<let N: Int> {
  typealias A = Vec<(N.magnitude), Int>
  // expected-error@-1 {{type 'N' has no member 'magnitude'}}
  // expected-error@-2 {{generic value must be an integer literal expression}}
}

// An associated type inherited from another protocol resolves without a cycle.
protocol InheritedMemberBase { associatedtype Inner }
protocol ParenInheritedMember: InheritedMemberBase {
  associatedtype B: Container<(Inner)>
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

// A concrete root resolves without the requirement signature, so a value member
// of one folds to its value inside a protocol too.
protocol ConcreteValueMemberRequirement {
  associatedtype X where X == Vec<(Limits.count), Int>
}

// A dependent member below the top level of a tuple.
protocol ParenTupleDependentMember {
  associatedtype A: Base
  associatedtype B: Container<(A.Inner, Self)>
}

// A value member of a dependent root cannot be told apart from a member type
// without the base's conformances, so it folds to a member type and is
// diagnosed as one.
protocol HasMax { static var max: Int { get } }
protocol ParenDependentValueMember {
  associatedtype A: HasMax
  associatedtype X where X == Vec<(A.max), Int>
  // expected-error@-1 {{cannot pass type 'Self.A.max' as a value for generic value 'N'}}
}

// An operator is not a type name, and building a member type repr for one trips
// an assertion. The fold has to decline, which leaves the cycle in place.
protocol ParenOperatorMember { // expected-error 2 {{circular reference}}
  associatedtype A: Base
  associatedtype B: Container<(A.+)>
  // expected-error@-1 {{cannot find operator '.+' in scope}}
  // expected-error@-2 {{generic value must be an integer literal expression}}
  // expected-note@-3 2 {{while resolving type 'Container<(A.+)>'}}
  // expected-note@-4 2 {{while resolving type '(A.+)'}}
}

// Outside a protocol, a nearer value still outranks a type of the same name in
// an enclosing scope.
enum Count {}
struct ShadowedByValue {
  static let Count = 4
  typealias A = Vec<(Count), Int>
}
