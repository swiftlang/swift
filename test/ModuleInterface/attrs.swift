// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name attrs \
// RUN:  -emit-private-module-interface-path %t.private.swiftinterface \
// RUN:  -enable-experimental-feature ABIAttribute \
// RUN:  -enable-experimental-feature ExecutionAttribute

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs
// RUN: %target-swift-typecheck-module-from-interface(%t.private.swiftinterface) -module-name attrs

// RUN: %FileCheck %s --check-prefixes CHECK,PUBLIC-CHECK --input-file %t.swiftinterface
// RUN: %FileCheck %s --check-prefixes CHECK,PRIVATE-CHECK --input-file %t.private.swiftinterface

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

@abi(func __abi__abiAttrOnFunction(param: Int))
public func abiAttrOnFunction(param: Int) {}
// CHECK: #if {{.*}} $ABIAttribute
// CHECK: @abi(func __abi__abiAttrOnFunction(param: Swift.Int))
// CHECK: public func abiAttrOnFunction(param: Swift.Int)
// CHECK: #else
// CHECK: @_silgen_name("$s5attrs07__abi__B14AttrOnFunction5paramySi_tF")
// CHECK: public func abiAttrOnFunction(param: Swift.Int)
// CHECK: #endif

@abi(let __abi__abiAttrOnVar: Int)
public var abiAttrOnVar: Int = 42
// CHECK: #if {{.*}} $ABIAttribute
// CHECK: @abi(var __abi__abiAttrOnVar: Swift.Int)
// CHECK: public var abiAttrOnVar: Swift.Int
// CHECK: #else
// CHECK: @available(*, unavailable, message: "this compiler cannot match the ABI specified by the @abi attribute")
// CHECK: public var abiAttrOnVar: Swift.Int
// CHECK: #endif

public struct MutatingTest {
  // CHECK: #if {{.*}} $ABIAttribute
  // CHECK: @abi(mutating func abiMutFunc())
  // CHECK: public mutating func abiMutFunc()
  // CHECK: #else
  // CHECK: @_silgen_name("$s5attrs12MutatingTestV10abiMutFuncyyF")
  // CHECK: public mutating func abiMutFunc()
  // CHECK: #endif
  @abi(mutating func abiMutFunc())
  public mutating func abiMutFunc() {}
}

// PUBLIC-CHECK-NOT: #if {{.*}} $ABIAttribute
// PUBLIC-CHECK-NOT: @abi(func abiSpiFunc())
// PUBLIC-CHECK-NOT: public func abiSpiFunc()
// PUBLIC-CHECK-NOT: #else
// PUBLIC-CHECK-NOT: @_silgen_name("$s5attrs10abiSpiFuncyyF")
// PUBLIC-CHECK-NOT: public func abiSpiFunc()
// PUBLIC-CHECK-NOT: #endif
// PRIVATE-CHECK: #if {{.*}} $ABIAttribute
// PRIVATE-CHECK: @abi(func abiSpiFunc())
// PRIVATE-CHECK: public func abiSpiFunc()
// PRIVATE-CHECK: #else
// PRIVATE-CHECK: @_silgen_name("$s5attrs10abiSpiFuncyyF")
// PRIVATE-CHECK: public func abiSpiFunc()
// PRIVATE-CHECK: #endif
@abi(func abiSpiFunc())
@_spi(spiGroup) public func abiSpiFunc() {}

@execution(concurrent)
public func testExecutionConcurrent() async {}
// CHECK: @execution(concurrent) public func testExecutionConcurrent() async

@execution(caller)
public func testExecutionCaller() async {}
// CHECK: @execution(caller) public func testExecutionCaller() async

// CHECK-NOT: @extensible
// CHECK: public enum TestExtensible
@extensible
public enum TestExtensible {
  case a
  case b
}
