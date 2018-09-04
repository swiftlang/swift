// RUN: %empty-directory(%t)
// R UN: %target-swift-frontend -emit-module -o %t/Test~partial.swiftmodule -module-name Test -primary-file %s
// R UN: %target-swift-frontend -merge-modules -emit-module -o %t/Test.swiftmodule %t/Test~partial.swiftmodule
// R UN: %target-swift-ide-test -print-module -module-to-print=Test -source-filename=x -I %t | %FileCheck %s

// RUN: %target-swift-frontend -emit-interface-path %t.swiftinterface -enable-resilience -emit-module -o /dev/null %s
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: struct MyStruct {{{$}}
public struct MyStruct {
// CHECK-NEXT: var publicVar: Int{{$}}
  public var publicVar: Int

// CHECK-NEXT: let publicLet: Bool{{$}}
  public let publicLet: Bool

// CHECK-NEXT: internal var _: String{{$}}
  var internalVar: String

// CHECK-NEXT: internal let _: Bool{{$}}
  let internalLet: Bool

// CHECK-NEXT: private var _: String{{$}}
  private var privateVar: String

// CHECK-NEXT: private let _: Bool{{$}}
  private let privateLet: Bool

// CHECK-NOT: private var
  private var computedPrivateVar: String {
    return "computedPrivateVar"
  }

// CHECK-NOT: private static var
  private static var staticPrivateVar: String = ""

// CHECK: }{{$}}
}

// CHECK: class MyClass {{{$}}
public class MyClass {
// CHECK-NEXT: public var publicVar: Int{{$}}
  public var publicVar: Int = 0

// CHECK-NEXT: public let publicLet: Bool{{$}}
  public let publicLet: Bool = true

// CHECK-NEXT: internal var _: String{{$}}
  var internalVar: String = ""

// CHECK-NEXT: internal let _: Bool{{$}}
  let internalLet: Bool = true

// CHECK-NEXT: private var _: String{{$}}
  private var privateVar: String = ""

// CHECK-NEXT: private let _: Bool{{$}}
  private let privateLet: Bool = true

// CHECK-NOT: private var
  private var computedPrivateVar: String {
    return "computedPrivateVar"
  }

// CHECK-NOT: private static var
  private static var staticPrivateVar: String = ""

// CHECK: }{{$}}
}
