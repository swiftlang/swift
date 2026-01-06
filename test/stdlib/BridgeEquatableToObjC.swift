// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest
import Foundation

var BridgeEquatableToObjC = TestSuite("BridgeEquatableToObjC")

struct MyEquatableStruct: Equatable {
  var text: String
}

struct MyNonEquatableStruct {
  var text: String
}

class MyEquatableClass: Equatable {
  static func == (lhs: MyEquatableClass, rhs: MyEquatableClass) -> Bool { true }
}

BridgeEquatableToObjC.test("Bridge equatable struct") {
  let swiftA = MyEquatableStruct(text: "xABC")
  let swiftB = swiftA
  let swiftResult = swiftA == swiftB

  let objcA = swiftA as AnyObject
  let objcB = swiftB as AnyObject
  let objcResult = objcA.isEqual(objcB)

  expectEqual(swiftResult, true)
  expectEqual(objcResult, true)
}

BridgeEquatableToObjC.test("Bridge non-equatable struct") {
  let swiftA = MyNonEquatableStruct(text: "xABC")
  let swiftB = swiftA

  let objcA = swiftA as AnyObject
  let objcB = swiftB as AnyObject
  let objcResult = objcA.isEqual(objcB)

  expectEqual(objcResult, false)
}

BridgeEquatableToObjC.test("Compare tagged pointer to equatable SwiftObject") {
  let literal = "The quick brown fox jumps over the lazy dog"
  let bridgedLiteral = literal as NSString
  let foo = MyEquatableClass()
  let bridgedFoo = foo as AnyObject
  let result = bridgedFoo.isEqual(bridgedLiteral)
  expectEqual(result, false)
}

runAllTests()
