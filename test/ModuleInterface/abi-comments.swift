// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/ABIComments.swiftinterface -module-name ABIComments -abi-comments-in-module-interface %s

// RUN: %FileCheck %s < %t/ABIComments.swiftinterface

// CHECK: // MANGLED NAME: $s11ABIComments11intToStringySSSiF
// CHECK-NEXT: public func intToString(_ value: Swift.Int) -> Swift.String
public func intToString(_ value: Int) -> String { "\(value)" }

// CHECK: // MANGLED NAME: $s11ABIComments13globalCounterSivp
// CHECK-NEXT: @_hasInitialValue public var globalCounter: Swift.Int
public var globalCounter: Int = 23

// CHECK: // MANGLED NAME: $s11ABIComments8MyStructVMa
// CHECK-NEXT: public struct MyStruct {
public struct MyStruct {
  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV6methodSiyF
  // CHECK-NEXT: public func method() -> Swift.Int
  public func method() -> Int { 5 }

  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV7counterSivp
  // CHECK-NEXT: @_hasInitialValue public var counter: Swift.Int
  public var counter: Int = 23

  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV11constantNumSivp
  // CHECK-NEXT: @_hasInitialValue public let constantNum: Swift.Int
  public let constantNum: Int = 23

  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV9staticLetSivpZ
  // CHECK-NEXT: @_hasInitialValue public static let staticLet: Swift.Int
  public static let staticLet: Int = 23

  // CHECK: // MANGLED NAME: $s11ABIComments8MyStructV8computedSivp
  // CHECK-NEXT: public var computed: Swift.Int {
  // CHECK-NEXT: // MANGLED NAME: $s11ABIComments8MyStructV8computedSivg
  // CHECK-NEXT: get
  // CHECK-NEXT: }
  public var computed: Int {
    13
  }
}
