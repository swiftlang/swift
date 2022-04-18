// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Module.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.swiftinterface)
// RUN: %FileCheck %s < %t/Module.swiftinterface

// REQUIRES: OS=macosx

@available(macOS 10.16, *)
public func introduced10_16() { }
// CHECK: @available(macOS 11.0, *)
// CHECK-NEXT: public func introduced10_16()


@available(OSX 11.0, *)
public func introduced11_0() { }
// CHECK-NEXT: @available(macOS 11.0, *)
// CHECK-NEXT: public func introduced11_0()


