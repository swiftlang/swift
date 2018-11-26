// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-interface-path %t-resilient.swiftinterface -enable-resilience -emit-module -o /dev/null %s
// RUN: %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-resilience -emit-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON


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

// CHECK-NEXT: internal var _: [[INT64]]{{$}}
// RESILIENT-NOT: internal var _: [[INT64]]{{$}}
  var internalVar: Int64 = 0

// CHECK-NEXT: internal let _: [[BOOL]]{{$}}
// RESILIENT-NOT: internal let _: [[BOOL]]{{$}}
  let internalLet: Bool = true

// CHECK-NEXT: private var _: [[INT64]]{{$}}
// RESILIENT-NOT: private var _: [[INT64]]{{$}}
  private var privateVar: Int64 = 0

// CHECK-NEXT: private let _: [[BOOL]]{{$}}
// RESILIENT-NOT: private let _: [[BOOL]]{{$}}
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
