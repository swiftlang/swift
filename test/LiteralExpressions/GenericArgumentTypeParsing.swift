// REQUIRES: swift_feature_LiteralExpressions

// Parenthesized generic arguments that must be parsed as types, not as
// generic value expressions.
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip

protocol P {}
struct S: P {}
struct G<T> {}
struct Pair<T, U> {}
protocol Container<Element> { associatedtype Element }
struct Box<E>: Container { typealias Element = E }

// =============================================================================
// Function types
//
// The parentheses open a function type, not a value expression, so the '->'
// must be consumed before the generic argument list's closing '>'.
// =============================================================================

func functionTypeInSignature(_ x: G<(Int, Int) -> Bool>) -> G<(Int) -> Void> {
  return G<(Int) -> Void>()
}

func functionTypeInExpression() {
  _ = G<(Int, Int) -> Bool>()
  _ = G<(Int, Int) -> Bool>.self
  _ = G<((Int) -> Int) -> Bool>()
  _ = G<(Int, Int) throws -> Bool>()
}

let functionTypeAlongsideValue: Pair<(Int, Int) -> Bool, (2 + 3)>? = nil
// expected-error@-1 {{cannot use value type '5' for generic argument 'U'}}

// =============================================================================
// Opaque 'some' types
//
// An opaque type must reach the enclosing declaration as a TypeRepr, because
// OpaqueResultTypeRequest collects it by walking the declaration's TypeRepr
// tree. Parsing the group as an expression hides it and resolution fails with
// "'some' types are only permitted in properties, subscripts, and functions".
// =============================================================================

var opaqueAlone: G<(some P)> { G<S>() }
var opaqueInTuple: G<(Int, some P)> { G<(Int, S)>() }
var opaqueInNestedTuple: G<((Int, some P))> { G<(Int, S)>() }

func opaqueInParameter(_ x: G<(some P)>) {}
func opaqueInResult() -> G<(Int, some P)> { G<(Int, S)>() }

// 'any' and protocol compositions recover from the expression path, but check
// that they keep working here.
var existentialInTuple: G<(Int, any P)>? { nil }

// =============================================================================
// Tuple types
//
// A top-level comma makes the group a tuple type. It is never a generic value
// argument, so routing it to the expression parser produced a spurious
// 'circular reference' when the tuple mentioned Self or an associated type.
// =============================================================================

let tupleArgument: G<(Int, String)> = G<(Int, String)>()
let parenthesizedArgument: G<(Int)> = G<Int>()

protocol SelfReferencingTuple {
  associatedtype A
  associatedtype B: Container<(A, Self)>
}

struct ConformsToSelfReferencingTuple: SelfReferencingTuple {
  typealias A = Int
  typealias B = Box<(Int, ConformsToSelfReferencingTuple)>
}

// =============================================================================
// Value expressions still take the expression path
// =============================================================================

let sum: InlineArray<(2 + 3), Int> = [1, 2, 3, 4, 5]
let sugar: [(3 * 2) of Int] = [1, 2, 3, 4, 5, 6]
let valueAlongsideFunctionType: InlineArray<(2 + 3), (Int, Int) -> Bool>? = nil
