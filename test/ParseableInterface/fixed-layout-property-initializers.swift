// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t-resilient.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s

// CHECK: @_fixed_layout public struct MyStruct {
@_fixed_layout
public struct MyStruct {
  // CHECK: public var publicVar: [[BOOL:(Swift\.)?Bool]] = false
  public var publicVar: Bool = false

  // CHECK: internal var internalVar: ([[BOOL]], [[BOOL]]) = (false, true)
  internal var internalVar: (Bool, Bool) = (false, true)

  // CHECK: private var privateVar: [[BOOL]] = Bool(4 < 10)
  private var privateVar: Bool = Bool(4 < 10)

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // CHECK: public var multiVar1: [[BOOL]] = Bool(false), (multiVar2, multiVar3): ([[BOOL]], [[BOOL]]) = (true, 3 == 0)
  public var multiVar1: Bool = Bool(false), (multiVar2, multiVar3): (Bool, Bool) = (true, 3 == 0)

  // CHECK: @_hasInitialValue public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // CHECK: @inlinable internal init() {}
  @inlinable init() {}
}

// CHECK: @_fixed_layout public class MyClass {
@_fixed_layout
public class MyClass {
  // CHECK: public var publicVar: [[BOOL]] = false
  public var publicVar: Bool = false

  // CHECK: internal var internalVar: [[BOOL]] = false
  internal var internalVar: Bool = false

  // CHECK: private var privateVar: {{(Swift\.)?}}UInt8 = UInt8(2)
  private var privateVar: UInt8 = UInt8(2)

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // CHECK: @_hasInitialValue public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // CHECK: @inlinable internal init() {}
  @inlinable init() {}
}

// CHECK: @_hasInitialValue public var topLevelVar: [[BOOL]]
public var topLevelVar: Bool = Bool(false && !true)
