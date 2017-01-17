//===--- TranscodedView.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var testTranscodedView = TestSuite("TranscodedView")

// For testing purposes only
extension UTF8.EncodedScalar {
  init(_ array: Array<UInt8>) {
    guard array.count <= 4 else { fatalError("too big!") }
    switch UTF8.parse1Forward(array) {
    case .valid(let value, _):
      self = value
      return
    default: fatalError("couldn't parse code units")
    }
  }

  var bits: UInt32 {
    let high = UInt32(self[3])
    let midHigh = UInt32(self[2])
    let midLow = UInt32(self[1])
    let low = UInt32(self[0])
    return low | (midLow << 8) | (midHigh << 16) | (high << 24)
  }
}
extension UTF16.EncodedScalar {
  init(_ array: Array<UInt16>) {
    guard array.count <= 2 else { fatalError("too big!") }
    switch UTF16.parse1Forward(array) {
    case .valid(let value, _):
      self = value
      return
    default: fatalError("couldn't parse code units")
    }
  }

  var bits: UInt32 {
    let high = UInt32(self[1])
    let low = UInt32(self[0])
    return low | (high << 16)
  }
}

extension UTF32.EncodedScalar {
  var bits: UInt32 {
    return self[0]
  }
}

// Test some kanji using raw code units
let 朝UTF8Raw: Array<UInt8> = [0xE6, 0x9C, 0x9D]
let 朝UTF16Raw: Array<UInt16> = [0x671D]
let 朝ごはんUTF8Raw: Array<UInt8> =
  朝UTF8Raw + [0xE3, 0x81, 0x94, 0xE3, 0x81, 0xAF, 0xE3, 0x82, 0x93]
let 朝ごはんUTF16Raw: Array<UInt16> =
  朝UTF16Raw + [0x3054, 0x306F, 0x3093]

// Simple test of scalar transcoding of ASCII
testTranscodedView.test("Simple ASCII transcoding") {
  let A: UInt8 = 65
  let S: UInt8 = 83
  let C: UInt8 = 67
  let I: UInt8 = 73
  let asciiRawCodeUnits: Array<UInt8> = [A, S, C, I, I]

  // Test the prototype
  let 朝UTF8Scalar = UTF8.EncodedScalar(朝UTF8Raw)
  let 朝UTF16Scalar = UTF16.EncodedScalar(朝UTF16Raw)

  expectEqual(朝UTF8Scalar.utf32.bits, 朝UTF16Scalar.utf32.bits)
  expectEqual(朝UTF8Scalar.utf16.bits, 朝UTF16Scalar.utf16.bits)
  expectEqual(朝UTF8Scalar.utf8.bits, 朝UTF16Scalar.utf8.bits)
}

// Test single-UTF16-code-unit transcoding of UTF8 to UTF16
testTranscodedView.test("TranscodedView") {
  let utf16View = TranscodedView(
    of: 朝ごはんUTF8Raw, from: UTF8.self, to: UTF16.self)

  var utf16Index = 朝ごはんUTF16Raw.startIndex
  for scalar in utf16View {
    expectEqual(type(of: scalar), UTF16.EncodedScalar.self)
    // FIXME: the below isn't a general way to test this, it's relying on
    // single-code-unit utf16 for Japanese text
    expectEqual(scalar.bits, UInt32(朝ごはんUTF16Raw[utf16Index]))
    utf16Index = utf16Index.advanced(by: 1)
  }
}

runAllTests()
