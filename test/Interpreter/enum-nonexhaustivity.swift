// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -swift-version 4 -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-run %t/main
// RUN: %target-build-swift %s -O -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -swift-version 4 -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-run %t/main
// RUN: %target-build-swift %s -Ounchecked -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -swift-version 4 -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-run %t/main
// REQUIRES: executable_test

import StdlibUnittest

var EnumTestSuite = TestSuite("Enums")

EnumTestSuite.test("PlainOldSwitch/NonExhaustive") {
  var gotCorrectValue = false
  switch getExpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    gotCorrectValue = true
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("TrapOnUnexpected/NonExhaustive") {
  // Must be built as Swift 4 to get the implicitly-inserted trap.
  expectCrashLater()
  switch getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("PlainOldSwitch/LyingExhaustive") {
  var gotCorrectValue = false
  switch getExpectedLiarValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    gotCorrectValue = true
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("TrapOnUnexpected/LyingExhaustive") {
  expectCrashLater()
  switch getUnexpectedLiarValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

@_exhaustive @objc enum SwiftEnum : Int32 {
  case A, B, C

  @inline(never) static func getExpectedValue() -> SwiftEnum {
    return .B
  }
  @inline(never) static func getUnexpectedValue() -> SwiftEnum {
    return unsafeBitCast(42 as Int32, to: SwiftEnum.self)
  }
}

EnumTestSuite.test("PlainOldSwitch/SwiftExhaustive") {
  var gotCorrectValue = false
  switch SwiftEnum.getExpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    gotCorrectValue = true
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("TrapOnUnexpected/SwiftExhaustive") {
  expectCrashLater()
  switch SwiftEnum.getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

runAllTests()
