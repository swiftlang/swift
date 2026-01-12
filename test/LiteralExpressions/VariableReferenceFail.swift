// Constant globals using @section initialized with literal expressions with simple variable references
// REQUIRES: swift_feature_LiteralExpressions

// RUN: %empty-directory(%t/deps)
// RUN: %target-swift-frontend -typecheck %s -enable-experimental-feature LiteralExpressions -verify

// This declaration is not itself declared to be a constant value
// but is referenced from one. Upon emitting a failure to constant fold, ensure
// we emit a note with a location of the declaration reference in a literal expression.
let foo: Int32 = 42 + Int32.random(in: 0..<10)
// expected-error@-1 {{not supported in a literal expression}}
@section("mysection") let bar: Int32 = 1 + foo
// expected-note@-1 {{requested from reference in a literal expression}}

// Currently, only the first encountered reference to a non-const non-constant-foldable
// declaration will result in a note, and subsequent encounters will emit a general failure
// to resolve the reference to a value
@section("mysection") let baz: Int32 = 1 + foo
// expected-error@-1 {{unable to resolve variable reference in a literal expression}}

