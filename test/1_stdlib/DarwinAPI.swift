// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

import Foundation

var DarwinBooleanAPI = TestSuite("DarwinBooleanAPI")

DarwinBooleanAPI.test("init") {
  do {
    let nativeTrue = true
    let true1 = DarwinBoolean(nativeTrue)
    let true2: DarwinBoolean = true
    expectEqual(1, unsafeBitCast(true1, UInt8.self))
    expectEqual(1, unsafeBitCast(true2, UInt8.self))
  }
  do {
    let nativeFalse = false
    let false1 = DarwinBoolean(nativeFalse)
    let false2: DarwinBoolean = false
    expectEqual(0, unsafeBitCast(false1, UInt8.self))
    expectEqual(0, unsafeBitCast(false2, UInt8.self))
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
  let otherValue = unsafeBitCast(rawValue, DarwinBoolean.self)
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
  let rawValue: UInt8 = 2
  let otherValue = unsafeBitCast(rawValue, DarwinBoolean.self)
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
