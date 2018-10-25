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
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: %target-run %t/a.out %S/Inputs/NormalizationTest.txt
// REQUIRES: executable_test

import Swift
import StdlibUnittest
import StdlibUnicodeUnittest

private func expectEqualIterators(
  label: String,
  expected: [UInt8],
  others: [String: [UInt8]],
  _ message: @autoclosure () -> String = "",
  showFrame: Bool = true,
  stackTrace: SourceLocStack = SourceLocStack(),
  file: String = #file,
  line: UInt = #line
) {
  let expectedString = String(decoding: expected, as: UTF8.self)
  let expectedCodeUnits = expectedString._nfcCodeUnits
  for (otherLabel, other) in others {
    let otherString = String(decoding: other, as: UTF8.self)
    expectEqual(
      expectedCodeUnits,
      otherString._nfcCodeUnits,
      "\(label) vs \(otherLabel)",
      stackTrace: stackTrace.pushIf(showFrame, file: file, line: line))
  }
}

var tests = TestSuite("StringNormalization")

tests.test("StringNormalization/ConvertToNFC")
.skip(.custom({
      if #available(macOS 10.14, iOS 12, watchOS 5, tvOS 12, *) { return false }
      return true
    }, reason: "NormalizationTest.txt requires Unicode 11"))
.code {
  for test in normalizationTests {
    expectEqualIterators(
      label: "NFC",
      expected: test.NFC,
      others: [
        "source": test.source,
        "NFC": test.NFC,
        "NFD": test.NFD
      ],
      stackTrace: SourceLocStack(test.loc))
  }
}

tests.test("StringNormalization/ConvertNFK*ToNFKC")
.skip(.custom({
      if #available(macOS 10.14, iOS 12, watchOS 5, tvOS 12, *) { return false }
      return true
    }, reason: "NormalizationTest.txt requires Unicode 11"))
.code {
  for test in normalizationTests {
    expectEqualIterators(
      label: "NFKC",
      expected: test.NFKC,
      others: [
        "NFKC": test.NFKC,
        "NFKD": test.NFKD
      ],
      stackTrace: SourceLocStack(test.loc))
  }
}

runAllTests()
