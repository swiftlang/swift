// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name abi_attr \
// RUN:  -parse-as-library -emit-private-module-interface-path %t.private.swiftinterface

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name abi_attr
// RUN: %target-swift-typecheck-module-from-interface(%t.private.swiftinterface) -module-name abi_attr

// RUN: %FileCheck %s --check-prefixes CHECK,PUBLIC-CHECK --input-file %t.swiftinterface \
// RUN:   --implicit-check-not '$ABIAttributeSE0479' --implicit-check-not '@_silgen_name'
// RUN: %FileCheck %s --check-prefixes CHECK,PRIVATE-CHECK --input-file %t.private.swiftinterface \
// RUN:   --implicit-check-not '$ABIAttributeSE0479' --implicit-check-not '@_silgen_name'

@abi(func __abi__abiAttrOnFunction(param: Int))
public func abiAttrOnFunction(param: Int) {}
// CHECK: @abi(func __abi__abiAttrOnFunction(param: Swift::Int))
// CHECK-NEXT: public func abiAttrOnFunction(param: Swift::Int)

@abi(let __abi__abiAttrOnVar: Int)
public var abiAttrOnVar: Int = 42
// CHECK: @abi(let __abi__abiAttrOnVar: Swift::Int)
// CHECK-NEXT: public var abiAttrOnVar: Swift::Int

public struct MutatingTest {
  // CHECK: @abi(mutating func abiMutFunc())
  // CHECK-NEXT: public mutating func abiMutFunc()
  @abi(mutating func abiMutFunc())
  public mutating func abiMutFunc() {}
}

// The counterpart in an @abi attribute must be printed as it was written: with
// its own introducer and without accessors.
public struct ComputedStorageTest {
  @usableFromInline internal var _x: Int

  // CHECK: @abi(var __abi__getOnly: Swift::Int){{$}}
  // CHECK-NEXT: public var getOnly: Swift::Int {
  @abi(var __abi__getOnly: Int)
  public var getOnly: Int { _x }

  // CHECK: @abi(var __abi__getSet: Swift::Int){{$}}
  // CHECK-NEXT: public var getSet: Swift::Int {
  @abi(var __abi__getSet: Int)
  public var getSet: Int {
    get { _x }
    set { _x = newValue }
  }

  // CHECK: @abi(subscript(__abi__getOnly i: Swift::Int) -> Swift::Int){{$}}
  // CHECK-NEXT: public subscript(i: Swift::Int) -> Swift::Int {
  @abi(subscript(__abi__getOnly i: Int) -> Int)
  public subscript(i: Int) -> Int { _x }

  // CHECK: @abi(subscript(__abi__getSet i: Swift::Int) -> Swift::Int){{$}}
  // CHECK-NEXT: public subscript(getSet i: Swift::Int) -> Swift::Int {
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
  // CHECK: @abi(var __abi__requirement: Swift::Int){{$}}
  // CHECK-NEXT: var requirement: Swift::Int { get }
  @abi(var __abi__requirement: Int)
  var requirement: Int { get }
}

// PUBLIC-CHECK-NOT: @abi(func abiSpiFunc())
// PUBLIC-CHECK-NOT: public func abiSpiFunc()
// PRIVATE-CHECK: @abi(func abiSpiFunc())
// PRIVATE-CHECK-NEXT: public func abiSpiFunc()
@abi(func abiSpiFunc())
@_spi(spiGroup) public func abiSpiFunc() {}

// We should print feature guards outside, but not inside, an @abi attribute.
@abi(func inlineArraySugarABI(_: [2 of Int]))
@available(SwiftStdlib 6.2, *)
public func inlineArraySugarABI(_: InlineArray<2, Int>) {}
// CHECK: #if compiler(>=5.3) && $InlineArrayTypeSugar
// CHECK-NEXT: @abi(func inlineArraySugarABI(_: [2 of Swift::Int]))
// CHECK-NEXT: @available
// CHECK-NEXT: public func inlineArraySugarABI(_: Swift::InlineArray<2, Swift::Int>)
// CHECK-NEXT: #endif
