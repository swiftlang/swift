// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix NONRESILIENT --check-prefix COMMON < %t.swiftinterface

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s --check-prefix FROMSOURCE --check-prefix RESILIENT --check-prefix COMMON < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix FROMMODULE --check-prefix NONRESILIENT --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t-resilient.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix FROMMODULE --check-prefix RESILIENT --check-prefix COMMON

// COMMON: @_fixed_layout public struct MyStruct {
@_fixed_layout
public struct MyStruct {
  // COMMON: public var publicVar: [[BOOL:(Swift\.)?Bool]] = false
  public var publicVar: Bool = false

  // COMMON: internal var internalVar: ([[BOOL]], [[BOOL]]) = (false, true)
  internal var internalVar: (Bool, Bool) = (false, true)

  // COMMON: private var privateVar: [[BOOL]] = Bool(4 < 10)
  private var privateVar: Bool = Bool(4 < 10)

  // COMMON: @usableFromInline
  // COMMON-NEXT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // COMMON: public var multiVar1: [[BOOL]] = Bool(false), (multiVar2, multiVar3): ([[BOOL]], [[BOOL]]) = (true, 3 == 0)
  public var multiVar1: Bool = Bool(false), (multiVar2, multiVar3): (Bool, Bool) = (true, 3 == 0)

  // NONRESILIENT: @_hasInitialValue public static var staticVar: [[BOOL]]
  // RESILIENT: {{^}}  public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // FROMSOURCE: @inlinable internal init() {}
  // FROMMODULE: @inlinable internal init(){{$}}
  @inlinable init() {}
}

// COMMON: @_fixed_layout public class MyClass {
@_fixed_layout
public class MyClass {
  // COMMON: public var publicVar: [[BOOL]] = false
  public var publicVar: Bool = false

  // COMMON: internal var internalVar: [[BOOL]] = false
  internal var internalVar: Bool = false

  // COMMON: private var privateVar: {{(Swift\.)?}}UInt8 = UInt8(2)
  private var privateVar: UInt8 = UInt8(2)

  // COMMON: @usableFromInline
  // COMMON-NEXT: internal var ufiVar: [[BOOL]] = true
  @usableFromInline internal var ufiVar: Bool = true

  // NONRESILIENT: @_hasInitialValue public static var staticVar: [[BOOL]]
  // RESILIENT: {{^}}  public static var staticVar: [[BOOL]]
  public static var staticVar: Bool = Bool(true && false)

  // FROMSOURCE: @inlinable internal init() {}
  // FROMMODULE: @inlinable internal init(){{$}}
  @inlinable init() {}
}

// NONRESILIENT: @_hasInitialValue public var topLevelVar: [[BOOL]]
// RESILIENT: {{^}}public var topLevelVar: [[BOOL]]
public var topLevelVar: Bool = Bool(false && !true)
