// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: @_transparent public func glass() -> Swift.Int { return 0 }{{$}}
@_transparent public func glass() -> Int { return 0 }

// CHECK: @_effects(readnone) public func illiterate(){{$}}
@_effects(readnone) public func illiterate() {}

// CHECK: @_effects(notEscaping arg1.**) public func escapeEffects(arg1: Swift.Int){{$}}
@_effects(notEscaping arg1.**) public func escapeEffects(arg1: Int) {}

// CHECK-LABEL: @frozen public struct Point {
@frozen public struct Point {
  // CHECK-NEXT: public var x: Swift.Int
  public var x: Int
  // CHECK-NEXT: public var y: Swift.Int
  public var y: Int
} // CHECK-NEXT: {{^}$}}

public func someGenericFunction<T>(_ t: T) -> Int { return 0 }

// CHECK: @_specialize(exported: true, kind: full, target: someGenericFunction(_:), where T == Swift.Int)
// CHECK: internal func __specialize_someGenericFunction<T>(_ t: T)
@_specialize(exported: true, target: someGenericFunction(_:), where T == Int)
internal func __specialize_someGenericFunction<T>(_ t: T) -> Int {
    fatalError("don't call")
}
