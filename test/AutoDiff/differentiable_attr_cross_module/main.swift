// Verify that `@differentiable` declarations can be differentiated from other
// modules.

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %S/../Inputs/differentiable_attr_other_module.swift %s -o /dev/null -lm
// NOTE(TF-892): `-lm` is necessary to prevent linker errors related to `ElementaryFunctions` on Ubuntu.

@differentiable(wrt: x)
func testInitializer(_ x: Float) -> Float {
  return Foo(x).x
}

@differentiable(wrt: foo)
func testMethod(_ foo: Foo) -> Float {
  return foo.method()
}

@differentiable(wrt: foo)
func testComputedProperty(_ foo: Foo) -> Float {
  return foo.computedProperty
}

@differentiable(wrt: foo)
func testSubscript(_ foo: Foo) -> Float {
  return foo[]
}
