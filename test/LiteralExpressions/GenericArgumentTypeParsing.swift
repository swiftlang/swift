// REQUIRES: swift_feature_LiteralExpressions

// Parenthesized generic arguments that must resolve as types, not as generic
// value expressions.
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions

struct G<T> {}
struct Pair<T, U> {}

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
// Value expressions still take the expression path
// =============================================================================

let sum: InlineArray<(2 + 3), Int> = [1, 2, 3, 4, 5]
let sugar: [(3 * 2) of Int] = [1, 2, 3, 4, 5, 6]
let valueAlongsideFunctionType: InlineArray<(2 + 3), (Int, Int) -> Bool>? = nil

// A tuple of values is not a generic value argument.
let tupleOfValues: InlineArray<(1, 2), Int> = [1]
// expected-error@-1 {{cannot convert value of type '(Int, Int)' to raw type 'Int'}}
// expected-error@-2 {{generic value must be an integer literal expression}}
