// SWIFT_ENABLE_TENSORFLOW
// Check that ASTScope lookup works for `@differentiable` attribute.

// NOTE(TF-815): Without custom scope support, ASTScopeLookup crashes for
// `@differentiable` attribute with where clauses on subscript and `var`
// declarations.

// RUN: %target-swift-frontend -typecheck %s -enable-astscope-lookup

struct Test<Element> {
  var element: Element
}
extension Test: Differentiable where Element: Differentiable {}
extension Test {
  @differentiable(where Element: Differentiable)
  init(_ element: Element) {
    self.element = element
  }

  @differentiable(where Element: Differentiable)
  func method() -> Element {
    element
  }

  @differentiable(where T: Differentiable)
  func method<T>(_ x: T) -> T {
    x
  }

  // NOTE(TF-815): This crashed without `DifferentiableAttributeScope` support.
  @differentiable(where Element: Differentiable)
  subscript(implicitGetterOnly_ : Void) -> Element {
    element
  }

  subscript(explicitGetterAndSetter _: Void) -> Element {
    @differentiable(where Element: Differentiable)
    get { element }
    set {}
  }

  // NOTE(TF-815): This crashed without `DifferentiableAttributeScope` support.
  @differentiable(where Element: Differentiable)
  var computedProperty: Element {
    element
  }

  var computedPropertyExplicitGetter: Element {
    @differentiable(where Element: Differentiable)
    get {
      element
    }
  }
}
