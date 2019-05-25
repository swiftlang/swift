// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: func testDifferentiableParam(f: @differentiable (Float) -> Float)
public func testDifferentiableParam(f: @differentiable (Float) -> Float) {}
