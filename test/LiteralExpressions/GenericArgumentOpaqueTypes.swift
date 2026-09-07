// REQUIRES: swift_feature_LiteralExpressions

// Opaque types inside a parenthesized generic argument.
//
// FIXME: SwiftParser has no 'some' rule in expression position, so it parses
// these as tuple expressions and round-trip verification fails with
// "unexpected code 'P' in tuple". Drop the flag once SwiftParser matches.
// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LiteralExpressions -disable-experimental-parser-round-trip

protocol P {}
struct S: P {}
struct G<T> {}

// An opaque type has to reach the enclosing declaration as a TypeRepr, because
// OpaqueResultTypeRequest collects it by walking that declaration's TypeRepr
// tree. The parser builds a TypeExpr for 'some P' even on the expression path,
// and the collector walks into the generic argument expression to find it.

var opaqueAlone: G<(some P)> { G<S>() }
var opaqueInTuple: G<(Int, some P)> { G<(Int, S)>() }
var opaqueInNestedTuple: G<((Int, some P))> { G<(Int, S)>() }

func opaqueInParameter(_ x: G<(some P)>) {}
func opaqueInResult() -> G<(Int, some P)> { G<(Int, S)>() }

// The opaque type is a real opaque parameter, so conflicting underlying types
// are still diagnosed.
var conflictingUnderlyingTypes: G<(some P)> {
  // expected-error@-1 {{function declares an opaque return type 'some P', but the return statements in its body do not have matching underlying types}}
  if Bool.random() {
    return G<S>() // expected-note {{return statement has underlying type 'S'}}
  }
  return G<S2>() // expected-note {{return statement has underlying type 'S2'}}
}
struct S2: P {}

// Ordinals stay correct when an opaque type on the expression path sits
// alongside one on the type path.
func twoOpaqueTypes() -> (G<(some P)>, some P) { (G<S>(), S()) }
func opaqueParameterAndArgument(_ x: some P, _ y: G<(some P)>) {}
