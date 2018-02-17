// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var StringOrderRelationTestSuite = TestSuite("StringOrderRelation")

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

runAllTests()

