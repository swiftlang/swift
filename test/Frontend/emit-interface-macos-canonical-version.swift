// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -enable-library-evolution -emit-module-interface-path %t/Module.swiftinterface -experimental-skip-non-inlinable-function-bodies
// RUN: %FileCheck %s --check-prefixes CHECK < %t/Module.swiftinterface

// REQUIRES: OS=macosx

@available(macOS 10.16, *)
public func introduced10_16() { }
// CHECK: @available(OSX 11.0, *)
// CHECK-NEXT: public func introduced10_16()


@available(OSX 11.0, *)
public func introduced11_0() { }
// CHECK-NEXT: @available(OSX 11.0, *)
// CHECK-NEXT: public func introduced11_0()


