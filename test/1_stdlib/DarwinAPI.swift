// RUN: %target-run-stdlib-swift
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
    expectEqual(1, true1.value)
    expectEqual(1, true2.value)
  }
  do {
    let nativeFalse = false
    let false1 = DarwinBoolean(nativeFalse)
    let false2: DarwinBoolean = false
    expectEqual(0, false1.value)
    expectEqual(0, false2.value)
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
  var otherValue: DarwinBoolean = false
  otherValue.value = 2
  expectTrue(otherValue.boolValue)
}

DarwinBooleanAPI.test("BooleanType") {
  var trueValue: DarwinBoolean = true
  expectIsBooleanType(&trueValue)

  var success = false
  if trueValue {
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
  var otherValue: DarwinBoolean = false
  otherValue.value = 2
  checkEquatable(true, trueValue, otherValue)
  checkEquatable(false, falseValue, otherValue)
}

DarwinBooleanAPI.test("&&") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false

  expectTrue(trueValue && trueValue)
  expectFalse(trueValue && falseValue)
  expectFalse(falseValue && trueValue)
  expectFalse(falseValue && falseValue)
}

DarwinBooleanAPI.test("||") {
  let trueValue: DarwinBoolean = true
  let falseValue: DarwinBoolean = false

  expectTrue(trueValue || trueValue)
  expectTrue(trueValue || falseValue)
  expectTrue(falseValue || trueValue)
  expectFalse(falseValue || falseValue)
}

runAllTests()
