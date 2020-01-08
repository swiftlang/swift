// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -o %t/differentiable_attr_serialization.swiftmodule
// RUN: %target-swift-frontend -merge-modules -sil-merge-partial-modules -emit-module %t/differentiable_attr_serialization.swiftmodule

// Test round-trip `@differentiable` attribute AST serialization.

// Motivation: check that `@differentiable` attributes always have original
// declaration set.

struct Foo: Differentiable {
  @differentiable
  func method() -> Self { self }

  @differentiable
  init(_ x: Float) {}

  @differentiable
  var computedProperty: Float { 1 }

  var computedPropertyGetter: Float {
    @differentiable
    get { 1 }
  }

  @differentiable
  subscript() -> Float { 1 }

  subscript(_ x: Float) -> Float {
    @differentiable
    get { 1 }
  }
}
