// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name abi_attr \
// RUN:  -parse-as-library -emit-private-module-interface-path %t.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name abi_attr
// RUN: %target-swift-typecheck-module-from-interface(%t.private.swiftinterface) -module-name abi_attr

// RUN: %FileCheck %s --check-prefixes CHECK,PUBLIC-CHECK --input-file %t.swiftinterface
// RUN: %FileCheck %s --check-prefixes CHECK,PRIVATE-CHECK --input-file %t.private.swiftinterface

@abi(func __abi__abiAttrOnFunction(param: Int))
public func abiAttrOnFunction(param: Int) {}
// CHECK: #if {{.*}} $ABIAttributeSE0479
// CHECK: @abi(func __abi__abiAttrOnFunction(param: Swift::Int))
// CHECK: public func abiAttrOnFunction(param: Swift::Int)
// CHECK: #else
// CHECK: @_silgen_name("$s8abi_attr02__a2__A14AttrOnFunction5paramySi_tF")
// CHECK: public func abiAttrOnFunction(param: Swift::Int)
// CHECK: #endif

@abi(let __abi__abiAttrOnVar: Int)
public var abiAttrOnVar: Int = 42
// CHECK: #if {{.*}} $ABIAttributeSE0479
// CHECK: @abi(let __abi__abiAttrOnVar: Swift::Int)
// CHECK: public var abiAttrOnVar: Swift::Int
// CHECK: #else
// CHECK: @available(*, unavailable, message: "this compiler cannot match the ABI specified by the @abi attribute")
// CHECK: public var abiAttrOnVar: Swift::Int
// CHECK: #endif

public struct MutatingTest {
  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK: @abi(mutating func abiMutFunc())
  // CHECK: public mutating func abiMutFunc()
  // CHECK: #else
  // CHECK: @_silgen_name("$s8abi_attr12MutatingTestV0A7MutFuncyyF")
  // CHECK: public mutating func abiMutFunc()
  // CHECK: #endif
  @abi(mutating func abiMutFunc())
  public mutating func abiMutFunc() {}
}

// The counterpart in an @abi attribute must be printed as it was written: with
// its own introducer and without accessors.
public struct ComputedStorageTest {
  @usableFromInline internal var _x: Int

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK-NEXT: @abi(var __abi__getOnly: Swift::Int){{$}}
  // CHECK-NEXT: public var getOnly: Swift::Int {
  // CHECK: #else
  // CHECK: @_silgen_name("$s8abi_attr19ComputedStorageTestV02__A9__getOnlySivg")
  // CHECK: #endif
  @abi(var __abi__getOnly: Int)
  public var getOnly: Int { _x }

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK-NEXT: @abi(var __abi__getSet: Swift::Int){{$}}
  // CHECK-NEXT: public var getSet: Swift::Int {
  // CHECK: #else
  // CHECK: @_silgen_name("$s8abi_attr19ComputedStorageTestV02__A8__getSetSivg")
  // CHECK: @_silgen_name("$s8abi_attr19ComputedStorageTestV02__A8__getSetSivs")
  // CHECK: #endif
  @abi(var __abi__getSet: Int)
  public var getSet: Int {
    get { _x }
    set { _x = newValue }
  }

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK-NEXT: @abi(subscript(__abi__getOnly i: Swift::Int) -> Swift::Int){{$}}
  // CHECK-NEXT: public subscript(i: Swift::Int) -> Swift::Int {
  // CHECK: #endif
  @abi(subscript(__abi__getOnly i: Int) -> Int)
  public subscript(i: Int) -> Int { _x }

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK-NEXT: @abi(subscript(__abi__getSet i: Swift::Int) -> Swift::Int){{$}}
  // CHECK-NEXT: public subscript(getSet i: Swift::Int) -> Swift::Int {
  // CHECK: #endif
  @abi(subscript(__abi__getSet i: Int) -> Int)
  public subscript(getSet i: Int) -> Int {
    get { _x }
    set { _x = newValue }
  }
}

// A `let` would be implicitly final, but these properties are not.
open class ComputedStorageClassTest {
  // CHECK: @abi(var __abi__computed: Swift::Int){{$}}
  // CHECK-NEXT: public var computed: Swift::Int {
  @abi(var __abi__computed: Int)
  public var computed: Int { 0 }

  // CHECK: @abi(var __abi__openComputed: Swift::Int){{$}}
  // CHECK-NEXT: open var openComputed: Swift::Int {
  @abi(var __abi__openComputed: Int)
  open var openComputed: Int { 0 }

  // CHECK: @abi(class var __abi__classComputed: Swift::Int){{$}}
  // CHECK-NEXT: public class var classComputed: Swift::Int {
  @abi(class var __abi__classComputed: Int)
  public class var classComputed: Int { 0 }

  // CHECK: @abi(final let __abi__storedLet: Swift::Int){{$}}
  // CHECK-NEXT: final public let storedLet: Swift::Int
  @abi(let __abi__storedLet: Int)
  public let storedLet: Int = 0
}

public protocol ComputedStorageRequirementTest {
  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK-NEXT: @abi(var __abi__requirement: Swift::Int){{$}}
  // CHECK-NEXT: var requirement: Swift::Int { get }
  // CHECK: #endif
  @abi(var __abi__requirement: Int)
  var requirement: Int { get }
}

// PUBLIC-CHECK-NOT: #if {{.*}} $ABIAttributeSE0479
// PUBLIC-CHECK-NOT: @abi(func abiSpiFunc())
// PUBLIC-CHECK-NOT: public func abiSpiFunc()
// PUBLIC-CHECK-NOT: #else
// PUBLIC-CHECK-NOT: @_silgen_name("$s8abi_attr0A7SpiFuncyyF")
// PUBLIC-CHECK-NOT: public func abiSpiFunc()
// PUBLIC-CHECK-NOT: #endif
// PRIVATE-CHECK: #if {{.*}} $ABIAttributeSE0479
// PRIVATE-CHECK: @abi(func abiSpiFunc())
// PRIVATE-CHECK: public func abiSpiFunc()
// PRIVATE-CHECK: #else
// PRIVATE-CHECK: @_silgen_name("$s8abi_attr0A7SpiFuncyyF")
// PRIVATE-CHECK: public func abiSpiFunc()
// PRIVATE-CHECK: #endif
@abi(func abiSpiFunc())
@_spi(spiGroup) public func abiSpiFunc() {}

// We should print feature guards outside, but not inside, an @abi attribute.
@abi(func sendingABI() -> sending Any?)
public func sendingABI() -> Any? { nil }
// CHECK: #if {{.*}} && $ABIAttributeSE0479
// CHECK: @abi(func sendingABI() -> sending Any?)
// CHECK: public func sendingABI() -> Any?
// CHECK: @_silgen_name("$s8abi_attr10sendingABIypSgyF")
// CHECK: public func sendingABI() -> Any?
// CHECK: #endif
