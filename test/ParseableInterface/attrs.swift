// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: @_transparent public func glass() -> Int { return 0 }{{$}}
@_transparent public func glass() -> Int { return 0 }

// CHECK: @_effects(readnone) public func illiterate(){{$}}
@_effects(readnone) public func illiterate() {}

// CHECK-LABEL: @_fixed_layout public struct Point {
@_fixed_layout public struct Point {
  // CHECK-NEXT: public var x: Int
  public var x: Int
  // CHECK-NEXT: public var y: Int
  public var y: Int
} // CHECK-NEXT: {{^}$}}
