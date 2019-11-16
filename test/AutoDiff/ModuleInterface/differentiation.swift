// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface -enable-library-evolution -enable-experimental-differentiable-programming %s
// RUN: %FileCheck %s < %t.swiftinterface

public func a(f: @differentiable (Float) -> Float) {}
// CHECK: public func a(f: @differentiable (Swift.Float) -> Swift.Float)

public func b(f: @differentiable(linear) (Float) -> Float) {}
// CHECK: public func b(f: @differentiable(linear) (Swift.Float) -> Swift.Float)

public func c(f: @differentiable (Float, @noDerivative Float) -> Float) {}
// CHECK: public func c(f: @differentiable (Swift.Float, @noDerivative Swift.Float) -> Swift.Float)
