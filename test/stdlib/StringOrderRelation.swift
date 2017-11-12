// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var StringOrderRelationTestSuite = TestSuite("StringOrderRelation")

StringOrderRelationTestSuite.test("StringOrderRelation/ASCII/NullByte")
  .xfail(.nativeRuntime("String comparison: ICU vs. Foundation " +
    "https://bugs.swift.org/browse/SR-630"))
  .code {
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

