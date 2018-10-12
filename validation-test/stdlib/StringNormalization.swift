//===--- StringNormalization.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/NormalizationTest.txt
// REQUIRES: executable_test

import Swift
import StdlibUnittest
import StdlibUnicodeUnittest

private func expectEqualIterators(expected: [UInt8], others: [[UInt8]]) {
  let expectedString = String(decoding: expected, as: UTF8.self)
  let expectedCodeUnits = expectedString._nfcCodeUnits
  
  for other in others {
    let otherString = String(decoding: other, as: UTF8.self)
    expectEqual(expectedCodeUnits, otherString._nfcCodeUnits)
  }
}

var tests = TestSuite("StringNormalization")

tests.test("StringNormalization/ConvertToNFC") {
  for test in normalizationTests {
    expectEqualIterators(expected: test.NFC, others: [test.source, test.NFC, test.NFD])
  }
}

tests.test("StringNormalization/ConvertNFK*ToNFKC") {
  for test in normalizationTests {
    expectEqualIterators(expected: test.NFKC, others: [test.NFKC, test.NFKD])
  }
}

runAllTests()
