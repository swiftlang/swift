// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

import _Differentiation

public func a(f: @differentiable(reverse) (Float) -> Float) {}
// CHECK: public func a(f: @differentiable(reverse) (Swift.Float) -> Swift.Float)

// TODO: Remove once `@differentiable` becomes deprecated.
public func b(f: @differentiable(reverse) (Float) -> Float) {}
// CHECK: public func b(f: @differentiable(reverse) (Swift.Float) -> Swift.Float)

public func c(f: @differentiable(reverse) (Float, @noDerivative Float) -> Float) {}
// CHECK: public func c(f: @differentiable(reverse) (Swift.Float, @noDerivative Swift.Float) -> Swift.Float)
