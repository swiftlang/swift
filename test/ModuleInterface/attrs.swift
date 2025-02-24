// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name attrs \
// RUN:  -enable-experimental-feature ABIAttribute \
// RUN:  -enable-experimental-feature ExecutionAttribute

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: swift_feature_ABIAttribute
// REQUIRES: swift_feature_ExecutionAttribute

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

@abi(public func __abi__abiAttrOnFunction(param: Int))
public func abiAttrOnFunction(param: Int) {}
// CHECK: #if {{.*}} $ABIAttribute
// CHECK: @abi(public func __abi__abiAttrOnFunction(param: Swift.Int))
// CHECK: public func abiAttrOnFunction(param: Swift.Int)
// CHECK: #else
// CHECK: @_silgen_name("$s5attrs07__abi__B14AttrOnFunction5paramySi_tF")
// CHECK: public func abiAttrOnFunction(param: Swift.Int)
// CHECK: #endif

@abi(public let __abi__abiAttrOnVar: Int)
public var abiAttrOnVar: Int = 42
// CHECK: #if {{.*}} $ABIAttribute
// CHECK: @abi(public var __abi__abiAttrOnVar: Swift.Int)
// CHECK: public var abiAttrOnVar: Swift.Int
// CHECK: #else
// CHECK: @available(*, unavailable, message: "this compiler cannot match the ABI specified by the @abi attribute")
// CHECK: public var abiAttrOnVar: Swift.Int
// CHECK: #endif

@execution(concurrent)
public func testExecutionConcurrent() async {}
// CHECK: @execution(concurrent) public func testExecutionConcurrent() async

@execution(caller)
public func testExecutionCaller() async {}
// CHECK: @execution(caller) public func testExecutionCaller() async
