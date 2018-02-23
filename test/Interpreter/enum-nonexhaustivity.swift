// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Onone -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-run %t/main
// RUN: %target-build-swift %s -O -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -Xfrontend -disable-objc-attr-requires-foundation-module
// RUN: %target-run %t/main
// RUN: %target-build-swift %s -Ounchecked -o %t/main -import-objc-header %S/Inputs/enum-nonexhaustivity.h -Xfrontend -disable-objc-attr-requires-foundation-module
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
  expectCrashLater()
  switch getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested/NonExhaustive") {
  expectCrashLater()
  switch (getExpectedValue(), getUnexpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    expectUnreachable()
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested2/NonExhaustive") {
  expectCrashLater()
  switch (getUnexpectedValue(), getExpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    expectUnreachable()
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("UnexpectedOkayNested/NonExhaustive") {
  var gotCorrectValue = false
  switch (getExpectedValue(), getUnexpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    gotCorrectValue = true
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("UnexpectedOkayNested2/NonExhaustive") {
  var gotCorrectValue = false
  switch (getUnexpectedValue(), getExpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    gotCorrectValue = true
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
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

EnumTestSuite.test("TrapOnUnexpectedNested/LyingExhaustive") {
  expectCrashLater()
  switch (getExpectedLiarValue(), getUnexpectedLiarValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    expectUnreachable()
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested2/LyingExhaustive") {
  expectCrashLater()
  switch (getUnexpectedLiarValue(), getExpectedLiarValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    expectUnreachable()
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("UnexpectedOkayNested/LyingExhaustive") {
  var gotCorrectValue = false
  switch (getExpectedLiarValue(), getUnexpectedLiarValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    gotCorrectValue = true
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("UnexpectedOkayNested2/LyingExhaustive") {
  var gotCorrectValue = false
  switch (getUnexpectedLiarValue(), getExpectedLiarValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    gotCorrectValue = true
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
}


@objc enum SwiftEnum : Int32 {
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

EnumTestSuite.test("TrapOnUnexpectedNested/SwiftExhaustive") {
  expectCrashLater()
  switch (SwiftEnum.getExpectedValue(), SwiftEnum.getUnexpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    expectUnreachable()
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested2/SwiftExhaustive") {
  expectCrashLater()
  switch (SwiftEnum.getUnexpectedValue(), SwiftEnum.getExpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    expectUnreachable()
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("UnexpectedOkayNested/SwiftExhaustive") {
  var gotCorrectValue = false
  switch (SwiftEnum.getExpectedValue(), SwiftEnum.getUnexpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (.B, _):
    gotCorrectValue = true
  case (.A, _), (.C, _):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("UnexpectedOkayNested2/SwiftExhaustive") {
  var gotCorrectValue = false
  switch (SwiftEnum.getUnexpectedValue(), SwiftEnum.getExpectedValue()) {
  case (.A, .A), (.C, .C):
    expectUnreachable()
  case (_, .B):
    gotCorrectValue = true
  case (_, .A), (_, .C):
    expectUnreachable()
  }
  expectTrue(gotCorrectValue)
}


runAllTests()
