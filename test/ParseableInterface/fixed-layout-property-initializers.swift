// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON < %t-resilient.swiftinterface

// FIXME(rdar44993525): %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// FIXME(rdar44993525): %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t-resilient.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON

// COMMON: @_fixed_layout public struct MyStruct {
@_fixed_layout
public struct MyStruct {
  // CHECK: @_hasInitialValue public var publicVar: [[BOOL:(Swift\.)?Bool]]{{$}}
  // RESILIENT: public var publicVar: [[BOOL:(Swift\.)?Bool]] = false
  public var publicVar: Bool = false

  // CHECK: @_hasInitialValue internal var internalVar: ([[BOOL]], [[BOOL]]){{$}}
  // RESILIENT: internal var internalVar: ([[BOOL]], [[BOOL]]) = (false, true)
  internal var internalVar: (Bool, Bool) = (false, true)

  // CHECK: @_hasInitialValue private var privateVar: [[BOOL]]{{$}}
  // RESILIENT: private var privateVar: [[BOOL]] = Bool(4 < 10)
  private var privateVar: Bool = Bool(4 < 10)

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal var ufiVar: [[BOOL]]{{$}}
  // RESILIENT: @usableFromInline
  // RESILIENT-NEXT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // CHECK: @_hasInitialValue public var multiVar1: [[BOOL]], (multiVar2, multiVar3): ([[BOOL]], [[BOOL]])
  // RESILIENT: public var multiVar1: [[BOOL]] = Bool(false), (multiVar2, multiVar3): ([[BOOL]], [[BOOL]]) = (true, 3 == 0)
  public var multiVar1: Bool = Bool(false), (multiVar2, multiVar3): (Bool, Bool) = (true, 3 == 0)

  // COMMON: @_hasInitialValue public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // COMMON: @inlinable internal init() {}
  @inlinable init() {}
}

// COMMON: @_fixed_layout public class MyClass {
@_fixed_layout
public class MyClass {
  // CHECK: @_hasInitialValue public var publicVar: [[BOOL]]{{$}}
  // RESILIENT: public var publicVar: [[BOOL]] = false
  public var publicVar: Bool = false

  // CHECK: @_hasInitialValue internal var internalVar: [[BOOL]]{{$}}
  // RESILIENT: internal var internalVar: [[BOOL]] = false
  internal var internalVar: Bool = false

  // CHECK: @_hasInitialValue private var privateVar: {{(Swift\.)?}}UInt8{{$}}
  // RESILIENT: private var privateVar: {{(Swift\.)?}}UInt8 = UInt8(2)
  private var privateVar: UInt8 = UInt8(2)

  // CHECK: @usableFromInline
  // CHECK-NEXT: internal var ufiVar: [[BOOL]]{{$}}
  // RESILIENT: @usableFromInline
  // RESILIENT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // COMMON: @_hasInitialValue public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // COMMON: @inlinable internal init() {}
  @inlinable init() {}
}

// COMMON: @_hasInitialValue public var topLevelVar: [[BOOL]]
public var topLevelVar: Bool = Bool(false && !true)
