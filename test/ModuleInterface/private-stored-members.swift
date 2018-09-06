// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface

// RUN: %target-swift-frontend -emit-interface-path %t-resilient.swiftinterface -enable-resilience -emit-module -o /dev/null %s
// RUN: %FileCheck %s --check-prefix RESILIENT < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// FIXME(rdar44144435): %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -emit-interface-path - | %FileCheck %s

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-resilience %t.swiftinterface -disable-objc-attr-requires-foundation-module
// FIXME(rdar44144435): %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -enable-resilience -emit-interface-path - | %FileCheck %s


// CHECK: struct MyStruct {{{$}}
// RESILIENT: struct MyStruct {{{$}}
public struct MyStruct {
// CHECK-NEXT: var publicVar: Int64{{$}}
// RESILIENT-NEXT: var publicVar: Int64{{$}}
  public var publicVar: Int64

// CHECK-NEXT: let publicLet: Bool{{$}}
// RESILIENT-NEXT: let publicLet: Bool{{$}}
  public let publicLet: Bool

// CHECK-NEXT: internal var _: Int64{{$}}
// RESILIENT-NOT: internal var _: Int64{{$}}
  var internalVar: Int64

// CHECK-NEXT: internal let _: Bool{{$}}
// RESILIENT-NOT: internal let _: Bool{{$}}
  let internalLet: Bool

// CHECK-NEXT: private var _: Int64{{$}}
// RESILIENT-NOT: private var _: Int64{{$}}
  private var privateVar: Int64

// CHECK-NEXT: private let _: Bool{{$}}
// RESILIENT-NOT: private let _: Bool{{$}}
  private let privateLet: Bool

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// CHECK-NEXT: var publicEndVar: Int64{{$}}
// RESILIENT-NEXT: var publicEndVar: Int64{{$}}
  public var publicEndVar: Int64 = 0

// CHECK: }{{$}}
// RESILIENT: }{{$}}
}

// CHECK: class MyClass {{{$}}
// RESILIENT: class MyClass {{{$}}
public class MyClass {
// CHECK-NEXT: var publicVar: Int64{{$}}
// RESILIENT-NEXT: var publicVar: Int64{{$}}
  public var publicVar: Int64 = 0

// CHECK-NEXT: let publicLet: Bool{{$}}
// RESILIENT-NEXT: let publicLet: Bool{{$}}
  public let publicLet: Bool = true

// CHECK-NEXT: internal var _: Int64{{$}}
// RESILIENT-NOT: internal var _: Int64{{$}}
  var internalVar: Int64 = 0

// CHECK-NEXT: internal let _: Bool{{$}}
// RESILIENT-NOT: internal let _: Bool{{$}}
  let internalLet: Bool = true

// CHECK-NEXT: private var _: Int64{{$}}
// RESILIENT-NOT: private var _: Int64{{$}}
  private var privateVar: Int64 = 0

// CHECK-NEXT: private let _: Bool{{$}}
// RESILIENT-NOT: private let _: Bool{{$}}
  private let privateLet: Bool = true

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// CHECK-NEXT: var publicEndVar: Int64{{$}}
// RESILIENT-NEXT: var publicEndVar: Int64{{$}}
  public var publicEndVar: Int64 = 0

  public init() {}

// CHECK: }{{$}}
// RESILIENT: }{{$}}
}
