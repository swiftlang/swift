//===--- ContiguouslyStored.swift -----------------------------------------===//
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


import StdlibUnittest

let suite = TestSuite("ContigououslyStored")

func testWithExistingUnsafeBuffer<S: Sequence>(_ s: S, contiguous: Bool) 
where S.SubSequence : Sequence {
  // Check that S and its slice report the same contiguity.
  expectEqual(contiguous, s.withExistingUnsafeBuffer { _ in true } ?? false)
  
  expectEqual(
    contiguous, s.dropFirst().withExistingUnsafeBuffer { _ in true } ?? false)

  if contiguous {
    // Check that s and its slice report the right memory relationship
    let r = s.withExistingUnsafeBuffer { whole in
      s.dropFirst().withExistingUnsafeBuffer { (part)->Bool in
        expectEqual(
          UnsafeRawPointer(whole.baseAddress! + 1),
          UnsafeRawPointer(part.baseAddress!))
        expectEqual(whole.count, part.count + 1)
        return true
      } ?? false
    } ?? false
    expectEqual(true, r)
  }
}

suite.test("Array") {
  testWithExistingUnsafeBuffer([2, 4, 7], contiguous: true)
}
suite.test("Range") {
  testWithExistingUnsafeBuffer(2...7, contiguous: false)
}
suite.test("_UTF16StringStorage") {
  testWithExistingUnsafeBuffer(_UTF16StringStorage(2...7), contiguous: true)
}
suite.test("_Latin1StringStorage") {
  testWithExistingUnsafeBuffer(_Latin1StringStorage(2...7), contiguous: true)
}
suite.test("UnsafeBufferPointer") {
  [2, 4, 7].withUnsafeBufferPointer {
    testWithExistingUnsafeBuffer($0, contiguous: true)
  }
}
suite.test("UnsafeMutableBufferPointer") {
  var a = [2, 4, 7]
  a.withUnsafeMutableBufferPointer {
    testWithExistingUnsafeBuffer($0, contiguous: true)
  }
}

runAllTests()
