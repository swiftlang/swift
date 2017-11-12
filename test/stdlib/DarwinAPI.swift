// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation

var DarwinBooleanAPI = TestSuite("DarwinBooleanAPI")

DarwinBooleanAPI.test("init") {
  do {
    let nativeTrue = true
    let true1 = DarwinBoolean(nativeTrue)
    let true2: DarwinBoolean = true
    expectEqual(1, unsafeBitCast(true1, to: UInt8.self))
    expectEqual(1, unsafeBitCast(true2, to: UInt8.self))
  }
  do {
    let nativeFalse = false
    let false1 = DarwinBoolean(nativeFalse)
    let false2: DarwinBoolean = false
    expectEqual(0, unsafeBitCast(false1, to: UInt8.self))
    expectEqual(0, unsafeBitCast(false2, to: UInt8.self))
  }
}

DarwinBooleanAPI.test("boolValue") {
  do {
    let nativeTrue = true
    let trueValue: DarwinBoolean = true
    expectEqual(nativeTrue, trueValue.boolValue)
  }

  do {
    let nativeFalse = false
    let falseValue: DarwinBoolean = false
    expectEqual(nativeFalse, falseValue.boolValue)
  }
}

DarwinBooleanAPI.test("boolValue/extra values") {
  let rawValue: UInt8 = 2
  let otherValue = unsafeBitCast(rawValue, to: DarwinBoolean.self)
  expectTrue(otherValue.boolValue)
}

DarwinBooleanAPI.test("Boolean") {
  var trueValue: DarwinBoolean = true

  var success = false
  if trueValue.boolValue {
    success = true
  } else {
    expectUnreachable()
  }
  expectTrue(success)
}

DarwinBooleanAPI.test("CustomStringConvertible") {
  let trueValue: DarwinBoolean = true
  expectEqual("true", trueValue.description)
  let falseValue: DarwinBoolean = false
  expectEqual("false", falseValue.description)
}

DarwinBooleanAPI.test("Equatable") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false
  checkEquatable(true, trueValue, trueValue)
  checkEquatable(true, falseValue, falseValue)
  checkEquatable(false, trueValue, falseValue)
  checkEquatable(false, falseValue, trueValue)
}

DarwinBooleanAPI.test("Equatable/extra values") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false
  let rawValue: UInt8 = 2
  let otherValue = unsafeBitCast(rawValue, to: DarwinBoolean.self)
  checkEquatable(true, trueValue, otherValue)
  checkEquatable(false, falseValue, otherValue)
}

DarwinBooleanAPI.test("&&") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false

  expectTrue(trueValue.boolValue && trueValue.boolValue)
  expectFalse(trueValue.boolValue && falseValue.boolValue)
  expectFalse(falseValue.boolValue && trueValue.boolValue)
  expectFalse(falseValue.boolValue && falseValue.boolValue)
}

DarwinBooleanAPI.test("||") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false

  expectTrue(trueValue.boolValue || trueValue.boolValue)
  expectTrue(trueValue.boolValue || falseValue.boolValue)
  expectTrue(falseValue.boolValue || trueValue.boolValue)
  expectFalse(falseValue.boolValue || falseValue.boolValue)
}

var DarwinIoctlConstants = TestSuite("DarwinIoctlConstants")

DarwinIoctlConstants.test("tty ioctl constants availability") {
  let aConstant = TIOCGWINSZ
} 

runAllTests()
