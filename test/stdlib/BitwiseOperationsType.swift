// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

struct MyInt32 : BitwiseOperations {
  var underlying: Int32

  static var allZeros: MyInt32 { return MyInt32(underlying: 0) }
}

func & (lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying & rhs.underlying)
}

func |(lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying | rhs.underlying)
}

func ^(lhs: MyInt32, rhs: MyInt32) -> MyInt32 {
  return MyInt32(underlying: lhs.underlying ^ rhs.underlying)
}

prefix func ~(x: MyInt32) -> MyInt32 {
  return MyInt32(underlying: ~x.underlying)
}

let BitwiseOperationsTests = TestSuite("BitwiseOperations")

BitwiseOperationsTests.test("smoke test") {
  var a = MyInt32(underlying: 0x3)
  a |= MyInt32(underlying: 0x4)
  expectEqual(0x7, a.underlying)

  a &= MyInt32(underlying: 0x5)
  expectEqual(0x5, a.underlying)

  a ^= MyInt32(underlying: 0x6)
  expectEqual(0x3, a.underlying)
}

runAllTests()

