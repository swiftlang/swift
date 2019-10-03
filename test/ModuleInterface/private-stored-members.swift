// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck %s < %t.swiftinterface --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t-resilient.swiftinterface -enable-library-evolution %s
// RUN: %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON < %t-resilient.swiftinterface

// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -module-name Test -emit-module-interface-path - | %FileCheck %s --check-prefix CHECK --check-prefix COMMON

// RUN: %target-swift-frontend -emit-module -o %t/TestResilient.swiftmodule -enable-library-evolution %t.swiftinterface -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/TestResilient.swiftmodule -module-name TestResilient -enable-library-evolution -emit-module-interface-path - | %FileCheck %s --check-prefix RESILIENT --check-prefix COMMON


// COMMON: struct MyStruct {{{$}}
public struct MyStruct {
// COMMON-NEXT: var publicVar: Swift.Int64{{$}}
  public var publicVar: Int64

// COMMON-NEXT: let publicLet: Swift.Bool{{$}}
  public let publicLet: Bool

// CHECK-NEXT: internal var internalVar: Swift.Int64{{$}}
// RESILIENT-NOT: internal var internalVar: Swift.Int64{{$}}
  var internalVar: Int64

// CHECK-NEXT: internal let internalLet: Swift.Bool{{$}}
// RESILIENT-NOT: internal let internalLet: Swift.Bool{{$}}
  let internalLet: Bool

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal var ufiVar: Swift.Int64{{$}}
  @usableFromInline var ufiVar: Int64

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal let ufiLet: Swift.Bool{{$}}
  @usableFromInline let ufiLet: Bool

// CHECK-NEXT: private var privateVar: Swift.Int64{{$}}
// RESILIENT-NOT: private var privateVar: Swift.Int64{{$}}
  private var privateVar: Int64

// CHECK-NEXT: private let privateLet: Swift.Bool{{$}}
// RESILIENT-NOT: private let privateLet: Swift.Bool{{$}}
  private let privateLet: Bool

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// COMMON: var publicEndVar: Swift.Int64{{$}}
  public var publicEndVar: Int64 = 0

// COMMON: }{{$}}
}

// COMMON: class MyClass {{{$}}
public class MyClass {
// COMMON-NEXT: var publicVar: Swift.Int64{{$}}
  public var publicVar: Int64 = 0

// COMMON-NEXT: let publicLet: Swift.Bool{{$}}
  public let publicLet: Bool = true

// CHECK-NEXT: internal var internalVar: Swift.Int64{{$}}
// RESILIENT-NOT: internal var internalVar: Swift.Int64{{$}}
  var internalVar: Int64 = 0

// CHECK-NEXT: internal let internalLet: Swift.Bool{{$}}
// RESILIENT-NOT: internal let internalLet: Swift.Bool{{$}}
  let internalLet: Bool = true

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal var ufiVar: Swift.Int64{{$}}
  @usableFromInline var ufiVar: Int64 = 0

// COMMON-NEXT: @usableFromInline
// COMMON-NEXT: internal let ufiLet: Swift.Bool{{$}}
  @usableFromInline let ufiLet: Bool = true

// CHECK-NEXT: private var privateVar: Swift.Int64{{$}}
// RESILIENT-NOT: private var privateVar: Swift.Int64{{$}}
  private var privateVar: Int64 = 0

// CHECK-NEXT: private let privateLet: Swift.Bool{{$}}
// RESILIENT-NOT: private let privateLet: Swift.Bool{{$}}
  private let privateLet: Bool = true

// CHECK-NOT: private var
// RESILIENT-NOT: private var
  private var computedPrivateVar: Int64 {
    return 0
  }

// CHECK-NOT: private static var
// RESILIENT-NOT: private static var
  private static var staticPrivateVar: Int64 = 0

// COMMON: var publicEndVar: Swift.Int64{{$}}
  public var publicEndVar: Int64 = 0

  public init() {}

// COMMON: }{{$}}
}
