// RUN: %target-run-simple-swift

import StdlibUnittest

var tests = TestSuite("FixedPoint")

func testBitwiseOperationsImpl<T : UnsignedIntegerType>(_: T.Type) {
  var x = numericCast(0b11_1010_00) as T
  var y = numericCast(0b10_1100_10) as T

  expectEqual(0b10_1000_00, x & y)
  expectEqual(0b11_1110_10, x | y)
  expectEqual(0b01_0110_10, x ^ y)
  expectEqual(0b00_0101_11, (~x) & 0xff)

  var z = T.allZeros
  expectEqual(x, x | z)
  expectEqual(x, x ^ z)
  expectEqual(z, x & z)
  expectEqual(x, x & ~z)
}

tests.test("BitwiseOperations/UInt8") {
  testBitwiseOperationsImpl(UInt8.self)
}

tests.test("BitwiseOperations/UInt16") {
  testBitwiseOperationsImpl(UInt16.self)
}

tests.test("BitwiseOperations/UInt32") {
  testBitwiseOperationsImpl(UInt32.self)
}

tests.test("BitwiseOperations/UInt64") {
  testBitwiseOperationsImpl(UInt64.self)
}

tests.test("OverflowCheck") {
  expectEqual(Int8.addWithOverflow(4, 5), (9, false))
  expectEqual(Int8.addWithOverflow(1, 127), (-128, true))
  expectEqual(UInt8.multiplyWithOverflow(2,128), (0, true))
}

tests.test("String.init") {
  let x: UInt32 = 0xdeadbeef
  expectEqual("efbeadde", String(x.bigEndian, radix: 16))

  let y = UInt32(bigEndian: 0xdeadbeef)
  expectEqual("deadbeef", String(y.bigEndian, radix: 16))
}

tests.test("byteSwapped") {
  expectEqual(288230376151711744, Int64(4).byteSwapped)
}

runAllTests()
