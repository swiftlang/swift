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

EnumTestSuite.test("TrapOnUnexpected/NonExhaustive")
    .crashOutputMatches("'NonExhaustiveEnum(rawValue: 3)'")
    .code {
  expectCrashLater()
  switch getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested/NonExhaustive")
    .crashOutputMatches("'(NonExhaustiveEnum, NonExhaustiveEnum)'")
    .code {
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

EnumTestSuite.test("TrapOnUnexpectedNested2/NonExhaustive")
    .crashOutputMatches("'(NonExhaustiveEnum, NonExhaustiveEnum)'")
    .code {
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

EnumTestSuite.test("TrapOnUnexpected/LyingExhaustive")
    .crashOutputMatches("'LyingExhaustiveEnum(rawValue: 3)'")
    .code {
  expectCrashLater()
  switch getUnexpectedLiarValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested/LyingExhaustive")
    .crashOutputMatches("'(LyingExhaustiveEnum, LyingExhaustiveEnum)'")
    .code {
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

EnumTestSuite.test("TrapOnUnexpectedNested2/LyingExhaustive")
    .crashOutputMatches("'(LyingExhaustiveEnum, LyingExhaustiveEnum)'")
    .code {
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
    return unsafeBitCast(-42 as Int32, to: SwiftEnum.self)
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

EnumTestSuite.test("TrapOnUnexpected/SwiftExhaustive")
    .crashOutputMatches("'SwiftEnum(rawValue: -42)'")
    .code {
  expectCrashLater()
  switch SwiftEnum.getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

EnumTestSuite.test("TrapOnUnexpectedNested/SwiftExhaustive")
    .crashOutputMatches("'(SwiftEnum, SwiftEnum)'")
    .code {
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

EnumTestSuite.test("TrapOnUnexpectedNested2/SwiftExhaustive")
    .crashOutputMatches("'(SwiftEnum, SwiftEnum)'")
    .code {
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

@inline(never)
func switchOnTwoThings<T>(_ a: T, _ b: SwiftEnum) {
  switch (a, b) {
  case (is String, _):
    expectUnreachable()
  case (_, .B):
    return
  case (_, .A), (_, .C):
    expectUnreachable()
  }
}

EnumTestSuite.test("Generic/Trap")
    .crashOutputMatches("'(Int, SwiftEnum)'")
    .code {
  expectCrashLater()
  switchOnTwoThings(1, SwiftEnum.getUnexpectedValue())
}

EnumTestSuite.test("Generic/Okay") {
  switchOnTwoThings(1, SwiftEnum.getExpectedValue())
}

@objc enum UnsignedSwiftEnum : UInt64 {
  case A, B, C

  @inline(never) static func getExpectedValue() -> UnsignedSwiftEnum {
    return .B
  }
  @inline(never) static func getUnexpectedValue() -> UnsignedSwiftEnum {
    return unsafeBitCast(~(0 as UInt64), to: UnsignedSwiftEnum.self)
  }
}

EnumTestSuite.test("PlainOldSwitch/LargeSwiftExhaustive") {
  var gotCorrectValue = false
  switch UnsignedSwiftEnum.getExpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    gotCorrectValue = true
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("TrapOnUnexpected/LargeSwiftExhaustive")
    .crashOutputMatches("'UnsignedSwiftEnum(rawValue: 18446744073709551615)'")
    .code {
  expectCrashLater()
  switch UnsignedSwiftEnum.getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

struct Outer {
  @objc enum NestedSwiftEnum: Int32 {
    case A, B, C

    @inline(never) static func getExpectedValue() -> NestedSwiftEnum {
      return .B
    }
    @inline(never) static func getUnexpectedValue() -> NestedSwiftEnum {
      return unsafeBitCast(-1 as Int32, to: NestedSwiftEnum.self)
    }
  }
}

EnumTestSuite.test("PlainOldSwitch/NestedSwiftExhaustive") {
  var gotCorrectValue = false
  switch Outer.NestedSwiftEnum.getExpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    gotCorrectValue = true
  }
  expectTrue(gotCorrectValue)
}

EnumTestSuite.test("TrapOnUnexpected/NestedSwiftExhaustive")
    .crashOutputMatches("'NestedSwiftEnum(rawValue: -1)'")
    .code {
  expectCrashLater()
  switch Outer.NestedSwiftEnum.getUnexpectedValue() {
  case .A, .C:
    expectUnreachable()
  case .B:
    expectUnreachable()
  }
  expectUnreachable()
}

runAllTests()
