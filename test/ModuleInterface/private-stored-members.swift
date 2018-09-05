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
// CHECK-NEXT: var publicVar: Int{{$}}
// RESILIENT-NEXT: var publicVar: Int{{$}}
  public var publicVar: Int

// CHECK-NEXT: let publicLet: Bool{{$}}
// RESILIENT-NEXT: let publicLet: Bool{{$}}
  public let publicLet: Bool

// CHECK-NEXT: internal var _: String{{$}}
// RESILIENT-NOT: internal var _: String{{$}}
  var internalVar: String

// CHECK-NEXT: internal let _: Bool{{$}}
// RESILIENT-NOT: internal let _: Bool{{$}}
  let internalLet: Bool

// CHECK-NEXT: private var _: String{{$}}
// RESILIENT-NOT: private var _: String{{$}}
  private var privateVar: String

// CHECK-NEXT: private let _: Bool{{$}}
// RESILIENT-NOT: private let _: Bool{{$}}
  private let privateLet: Bool

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: String {
    return "computedPrivateVar"
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: String = ""

// CHECK: }{{$}}
// RESILIENT: }{{$}}
}

// CHECK: class MyClass {{{$}}
// RESILIENT: class MyClass {{{$}}
public class MyClass {
// CHECK-NEXT: var publicVar: Int{{$}}
// RESILIENT-NEXT: var publicVar: Int{{$}}
  public var publicVar: Int = 0

// CHECK-NEXT: let publicLet: Bool{{$}}
// RESILIENT-NEXT: let publicLet: Bool{{$}}
  public let publicLet: Bool = true

// CHECK-NEXT: internal var _: String{{$}}
// RESILIENT-NOT: internal var _: String{{$}}
  var internalVar: String = ""

// CHECK-NEXT: internal let _: Bool{{$}}
// RESILIENT-NOT: internal let _: Bool{{$}}
  let internalLet: Bool = true

// CHECK-NEXT: private var _: String{{$}}
// RESILIENT-NOT: private var _: String{{$}}
  private var privateVar: String = ""

// CHECK-NEXT: private let _: Bool{{$}}
// RESILIENT-NOT: private let _: Bool{{$}}
  private let privateLet: Bool = true

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: String {
    return "computedPrivateVar"
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: String = ""

// CHECK: }{{$}}
// RESILIENT: }{{$}}
}
