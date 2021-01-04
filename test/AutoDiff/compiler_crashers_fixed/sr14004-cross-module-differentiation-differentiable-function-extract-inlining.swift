// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(Library)) -emit-module -emit-module-path %t/Library.swiftmodule -module-name Library -DLIBRARY %s
// RUN: %target-build-swift -I %t -O -emit-module %s

// SR-14004: Assertion failure due to function with `differentiable_function_extract`
// with explicit extractee type being deserialized into a raw SIL module.

#if LIBRARY

import _Differentiation

public struct Struct<Scalar>: Differentiable {}

@differentiable
public func foo<Scalar>(_ x: Struct<Scalar>) -> Struct<Scalar> { x }

@inlinable
@differentiable
public func bar<Scalar>(_ x: Struct<Scalar>) -> Struct<Scalar> {
  foo(x)
}

#else

import _Differentiation
import Library

public func foo(
  body: @differentiable (Struct<Float>) -> Struct<Float> = bar
) {
  fatalError()
}

#endif
