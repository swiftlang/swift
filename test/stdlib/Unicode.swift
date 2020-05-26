//===--- Unicode.swift ----------------------------------------------------===//
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
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest


var UnicodeInternals = TestSuite("UnicodeInternals")

UnicodeInternals.test("copy") {
  var u8: [UTF8.CodeUnit] = [ 0, 1, 2, 3, 4, 5 ]
  var u16: [UTF16.CodeUnit] = [ 6, 7, 8, 9, 10, 11 ]

  u16.withUnsafeMutableBufferPointer {
    (u16) -> () in
    let p16 = u16.baseAddress!

    u8.withUnsafeMutableBufferPointer {
      (u8) -> () in
      let p8 = u8.baseAddress!

      UTF16._copy(source: p8, destination: p16, count: 3)
      expectEqual([ 0, 1, 2, 9, 10, 11 ], Array(u16))

      UTF16._copy(source: p16 + 3, destination: p8, count: 3)
      expectEqual([ 9, 10, 11, 3, 4, 5 ], Array(u8))

      UTF16._copy(source: p16, destination: p16 + 3, count: 3)
      expectEqual([ 0, 1, 2, 0, 1, 2 ], Array(u16))

      UTF16._copy(source: p8, destination: p8 + 3, count: 3)
      expectEqual([ 9, 10, 11, 9, 10, 11 ], Array(u8))
    }
  }
}

var UnicodeAPIs = TestSuite("UnicodeAPIs")

UnicodeAPIs.test("UnicodeDecodingResult/Equatable") {
  let instances: [UnicodeDecodingResult] = [
    .scalarValue("a"),
    .scalarValue("b"),
    .emptyInput,
    .error
  ]
  checkEquatable(instances, oracle: ==)
}

typealias ASCII = Unicode.ASCII
typealias UTF8 = Unicode.UTF8
typealias UTF16 = Unicode.UTF16
typealias UTF32 = Unicode.UTF32

UnicodeAPIs.test("UTF-8 and UTF-16 queries") {
  guard #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) else {
    return
  }
  let str = "abéÏ01😓🎃👨‍👨‍👧‍👦アイウエオ"
  let scalars = Array(str.unicodeScalars)
  for scalar in scalars {
    expectEqual(String(scalar).utf16.count, UTF16.width(scalar))
    expectEqual(String(scalar).utf8.count, UTF8.width(scalar))

    expectEqual(UTF32.isASCII(scalar.value), UTF8.isASCII(scalar.utf8[0]))
    expectEqual(UTF32.isASCII(scalar.value), ASCII.isASCII(scalar.utf8[0]))
    expectEqual(UTF32.isASCII(scalar.value), UTF16.isASCII(scalar.utf16[0]))

    if scalar.utf16.count == 2 {
      let lead = scalar.utf16[0]
      let trail = scalar.utf16[1]
      expectTrue(UTF16.isLeadSurrogate(lead))
      expectTrue(UTF16.isSurrogate(lead))
      expectFalse(UTF16.isASCII(lead))

      expectTrue(UTF16.isTrailSurrogate(trail))
      expectTrue(UTF16.isSurrogate(trail))
    } else {
      let codeUnit = scalar.utf16[0]
      expectFalse(UTF16.isLeadSurrogate(codeUnit))
      expectFalse(UTF16.isTrailSurrogate(codeUnit))
      expectFalse(UTF16.isSurrogate(codeUnit))

      expectEqual(codeUnit <= 0x7F, UTF16.isASCII(codeUnit))
    }

    expectFalse(UTF8.isContinuation(scalar.utf8[0]))
    if scalar.utf8.count == 1 {
      let ascii = scalar.utf8[0]
      expectFalse(UTF8.isContinuation(ascii))
      expectTrue(UTF8.isASCII(ascii))
    } else {
      expectFalse(UTF8.isASCII(scalar.utf8[0]))
      expectFalse(UTF8.isContinuation(scalar.utf8[0]))
      for i in 1..<scalar.utf8.count {
        expectTrue(UTF8.isContinuation(scalar.utf8[i]))
        expectFalse(UTF8.isASCII(scalar.utf8[i]))
      }
    }
  }
}

runAllTests()
