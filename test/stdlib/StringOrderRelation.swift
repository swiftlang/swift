// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var StringOrderRelationTestSuite = TestSuite("StringOrderRelation")

fileprivate func expectLess(
  _ lhs: String,
  _ rhs: String,
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file, line: UInt = #line
) {
  expectTrue(lhs < rhs, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  expectFalse(lhs == rhs, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  expectFalse(lhs > rhs, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  expectFalse(rhs < lhs, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
  expectTrue(rhs > lhs, stackTrace: stackTrace, showFrame: showFrame, file: file, line: line)
}

StringOrderRelationTestSuite.test("StringOrderRelation/ASCII/NullByte") {
  let baseString = "a"
  let nullbyteString = "a\0"
  expectTrue(baseString < nullbyteString)
  expectTrue(baseString <= nullbyteString)
  expectFalse(baseString > nullbyteString)
  expectFalse(baseString >= nullbyteString)
  expectFalse(baseString == nullbyteString)
  expectTrue(baseString != nullbyteString)
}

// One string can be a prefix of the other only after normalization, in which
// case their UTF-8 lengths say nothing about which one has more scalars.
StringOrderRelationTestSuite.test("StringOrderRelation/NormalizedPrefix") {
  // U+037E normalizes to ";", and has the same UTF-8 length as ";A".
  expectLess("\u{37E}", ";A")
  // U+212A normalizes to "K", and has a longer UTF-8 encoding than "KA".
  expectLess("\u{212A}", "KA")
  // U+1FEF normalizes to "`", and has a longer UTF-8 encoding than "`A".
  expectLess("\u{1FEF}", "`A")

  expectTrue("\u{37E}" == ";")
  expectLess("a", "ab")
}

runAllTests()

