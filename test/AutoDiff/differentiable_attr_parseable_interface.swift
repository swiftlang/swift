// SWIFT_ENABLE_TENSORFLOW
// RUN: %target-swift-frontend -swift-version 5 -typecheck -emit-parseable-module-interface-path %t.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: func testDifferentiableParam(f: @differentiable (Swift.Float) -> Swift.Float)
public func testDifferentiableParam(f: @differentiable (Float) -> Float) {}
