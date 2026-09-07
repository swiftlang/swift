// REQUIRES: swift_feature_LiteralExpressions

// Generic arguments that reach the expression path but have to resolve as types.
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions

struct G<T> {}
protocol P {}
protocol Q {}

// =============================================================================
// Protocol compositions
//
// A parenthesized composition reaches the expression path as an unfolded
// SequenceExpr. The generic-argument simplifier folds it so that the
// composition is recognized as a type.
// =============================================================================

var compositionAlone: G<(P & Q)>? { nil }
var compositionInTuple: G<(Int, P & Q)>? { nil }
var existentialComposition: G<(any P & Q)>? { nil }
var existentialInTuple: G<(Int, any P)>? { nil }

// A bitwise '&' between values stays a value expression. 6 & 5 == 4.
let bitwiseAndValue: InlineArray<(6 & 5), Int> = [1, 2, 3, 4]
