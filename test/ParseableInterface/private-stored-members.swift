// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -typecheck -emit-parseable-module-interface-path %t-resilient.swiftinterface -enable-resilience %s
// RUN: %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-parseable-module-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON


// COMMON: struct MyStruct {{{$}}
public struct MyStruct {
// COMMON-NEXT: var publicVar: [[INT64:(Swift\.)?Int64]]{{$}}
  public var publicVar: Int64

// COMMON-NEXT: let publicLet: [[BOOL:(Swift\.)?Bool]]{{$}}
  public let publicLet: Bool

// CHECK-NEXT: internal var _: [[INT64]]{{$}}
// RESILIENT-NOT: internal var _: [[INT64]]{{$}}
  var internalVar: Int64

// CHECK-NEXT: internal let _: [[BOOL]]{{$}}
// RESILIENT-NOT: internal let _: [[BOOL]]{{$}}
  let internalLet: Bool

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal var ufiVar: [[INT64]]{{$}}
  @usableFromInline var ufiVar: Int64

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal let ufiLet: [[BOOL]]{{$}}
  @usableFromInline let ufiLet: Bool

// CHECK-NEXT: private var _: [[INT64]]{{$}}
// RESILIENT-NOT: private var _: [[INT64]]{{$}}
  private var privateVar: Int64

// CHECK-NEXT: private let _: [[BOOL]]{{$}}
// RESILIENT-NOT: private let _: [[BOOL]]{{$}}
  private let privateLet: Bool

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// COMMON: var publicEndVar: [[INT64]]{{$}}
  public var publicEndVar: Int64 = 0

// COMMON: }{{$}}
}

// COMMON: class MyClass {{{$}}
public class MyClass {
// COMMON-NEXT: var publicVar: [[INT64]]{{$}}
  public var publicVar: Int64 = 0

// COMMON-NEXT: let publicLet: [[BOOL]]{{$}}
  public let publicLet: Bool = true

// CHECK-NEXT: internal var internalVar: [[INT64]]{{$}}
// RESILIENT-NOT: internal var internalVar: [[INT64]]{{$}}
  var internalVar: Int64 = 0

// CHECK-NEXT: internal let internalLet: [[BOOL]]{{$}}
// RESILIENT-NOT: internal let internalLet: [[BOOL]]{{$}}
  let internalLet: Bool = true

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal var ufiVar: [[INT64]]{{$}}
  @usableFromInline var ufiVar: Int64 = 0

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal let ufiLet: [[BOOL]]{{$}}
  @usableFromInline let ufiLet: Bool = true

// CHECK-NEXT: private var privateVar: [[INT64]]{{$}}
// RESILIENT-NOT: private var privateVar: [[INT64]]{{$}}
  private var privateVar: Int64 = 0

// CHECK-NEXT: private let privateLet: [[BOOL]]{{$}}
// RESILIENT-NOT: private let privateLet: [[BOOL]]{{$}}
  private let privateLet: Bool = true

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// COMMON: var publicEndVar: [[INT64]]{{$}}
  public var publicEndVar: Int64 = 0

  public init() {}

// COMMON: }{{$}}
}
