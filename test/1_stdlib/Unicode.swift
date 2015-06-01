//===--- Unicode.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
    (u16)->() in
    let p16 = u16.baseAddress

    u8.withUnsafeMutableBufferPointer {
      (u8)->() in
      let p8 = u8.baseAddress

      UTF16._copy(p8, destination: p16, count: 3)
      expectEqual([ 0, 1, 2, 9, 10, 11 ], Array(u16))

      UTF16._copy(p16 + 3, destination: p8, count: 3)
      expectEqual([ 9, 10, 11, 3, 4, 5 ], Array(u8))

      UTF16._copy(p16, destination: p16 + 3, count: 3)
      expectEqual([ 0, 1, 2, 0, 1, 2 ], Array(u16))

      UTF16._copy(p8, destination: p8 + 3, count: 3)
      expectEqual([ 9, 10, 11, 9, 10, 11 ], Array(u8))
    }
  }
}

