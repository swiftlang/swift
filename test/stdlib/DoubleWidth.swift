// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// FIXME(double-width): <rdar://problem/32726173>
// REQUIRES: rdar32726173

import StdlibUnittest

var dwTests = TestSuite("DoubleWidth")

typealias UInt128 = DoubleWidth<UInt64>
typealias UInt256 = DoubleWidth<UInt128>
typealias UInt512 = DoubleWidth<UInt256>
typealias UInt1024 = DoubleWidth<UInt512>

typealias Int128 = DoubleWidth<Int64>
typealias Int256 = DoubleWidth<Int128>
typealias Int512 = DoubleWidth<Int256>
typealias Int1024 = DoubleWidth<Int512>

func checkSignedIntegerConformance<T: SignedInteger>(_ x: T) {}
func checkUnsignedIntegerConformance<T: UnsignedInteger>(_ x: T) {}

dwTests.test("Literals") {
  let w: DoubleWidth<UInt8> = 100
  expectTrue(w == 100 as Int)
  
  let x: DoubleWidth<UInt8> = 1000
  expectTrue(x == 1000 as Int)
  
  let y: DoubleWidth<Int8> = 1000
  expectTrue(y == 1000 as Int)
  
  let z: DoubleWidth<Int8> = -1000
  expectTrue(z == -1000 as Int)
  
  expectCrashLater()
  _ = -1 as DoubleWidth<UInt8>
}

dwTests.test("Literals/Large/Signed") {
  let a: Int256 =
    0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
  let b: Int256 =
    -0x8000000000000000000000000000000000000000000000000000000000000000
  expectEqual(a, Int256.max)
  expectEqual(b, Int256.min)
  expectCrashLater()
  _ = -0x8000000000000000000000000000000000000000000000000000000000000001
    as Int256
}

dwTests.test("Literals/Large/SignedOverflow") {
  expectCrashLater()
  _ = 0x8000000000000000000000000000000000000000000000000000000000000000
    as Int256
}

dwTests.test("Literals/Large/Unsigned") {
  let a: UInt256 =
    0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
  let b: UInt256 = 0
  expectEqual(a, UInt256.max)
  expectEqual(b, UInt256.min)
  expectCrashLater()
  _ = -1 as UInt256
}

dwTests.test("Literals/Large/UnsignedOverflow") {
  expectCrashLater()
  _ = 0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0
    as UInt256
}

dwTests.test("Arithmetic/unsigned") {
  let x: DoubleWidth<UInt8> = 1000
  let y: DoubleWidth<UInt8> = 1111
  expectEqual(x + 1, 1001)
  expectEqual(x + x, 2000)
  expectEqual(x - (1 as DoubleWidth<UInt8>), 999)
  expectEqual(x - x, 0)
  expectEqual(y - x, 111)

  expectEqual(x * 7, 7000)
  expectEqual(y * 7, 7777)

  expectEqual(x / 3, 333)
  expectEqual(x / x, 1)
  expectEqual(x / y, 0)
  expectEqual(y / x, 1)

  expectEqual(x % 3, 1)
  expectEqual(x % y, x)
}

dwTests.test("Arithmetic/signed") {
  let x: DoubleWidth<Int8> = 1000
  let y: DoubleWidth<Int8> = -1111
  expectEqual(x + 1, 1001)
  expectEqual(x + x, 2000)
  expectEqual(x - (1 as DoubleWidth<Int8>), 999)
  expectEqual(x - x, 0)
  expectEqual(0 - x, -1000)
  expectEqual(x + y, -111)
  expectEqual(x - y, 2111)

  expectEqual(x * 7, 7000)
  expectEqual(y * 7, -7777)
  expectEqual(x * -7, -7000)
  expectEqual(y * -7, 7777)

  expectEqual(x / 3, 333)
  expectEqual(x / -3, -333)
  expectEqual(x / x, 1)
  expectEqual(x / y, 0)
  expectEqual(y / x, -1)
  expectEqual(y / y, 1)

  expectEqual(x % 3, 1)
  expectEqual(x % -3, 1)
  expectEqual(y % 3, -1)
  expectEqual(y % -3, -1)

  expectEqual(-y, 1111)
  expectEqual(-x, -1000)
}

dwTests.test("Nested") {
  do {
    let x = UInt1024.max
    let (y, o) = x.addingReportingOverflow(1)
    expectEqual(y, 0)
    expectTrue(y == (0 as Int))
    expectTrue(o)
  }

  do {
    let x = Int1024.max
    let (y, o) = x.addingReportingOverflow(1)
    expectEqual(y, Int1024.min)
    expectLT(y, 0)
    expectTrue(y < (0 as Int))
    expectTrue(y < (0 as UInt))
    expectTrue(o)
  }

  expectFalse(UInt1024.isSigned)
  expectEqual(UInt1024.bitWidth, 1024)
  expectTrue(Int1024.isSigned)
  expectEqual(Int1024.bitWidth, 1024)

  expectEqualSequence(
    UInt1024.max.words, repeatElement(UInt.max, count: 1024 / UInt.bitWidth))
}

dwTests.test("inits") {
  typealias DWU16 = DoubleWidth<UInt8>

  expectTrue(DWU16(UInt16.max) == UInt16.max)
  expectNil(DWU16(exactly: UInt32.max))
  expectEqual(DWU16(truncatingIfNeeded: UInt64.max), DWU16.max)

  expectCrashLater()
  _ = DWU16(UInt32.max)
}

dwTests.test("TwoWords") {
  typealias DW = DoubleWidth<Int>

  expectEqual(-1 as DW, DW(truncatingIfNeeded: -1 as Int8))

  expectNil(Int(exactly: DW(Int.min) - 1))
  expectNil(Int(exactly: DW(Int.max) + 1))

  expectTrue(DW(Int.min) - 1 < Int.min)
  expectTrue(DW(Int.max) + 1 > Int.max)
}

dwTests.test("Bitshifts") {
  typealias DWU64 = DoubleWidth<DoubleWidth<DoubleWidth<UInt8>>>
  typealias DWI64 = DoubleWidth<DoubleWidth<DoubleWidth<Int8>>>

  func f<T: FixedWidthInteger, U: FixedWidthInteger>(_ x: T, type: U.Type) {
    let y = U(x)
    expectEqual(T.bitWidth, U.bitWidth)
    for i in -(T.bitWidth + 1)...(T.bitWidth + 1) {
      expectTrue(x << i == y << i)
      expectTrue(x >> i == y >> i)

      expectTrue(x &<< i == y &<< i)
      expectTrue(x &>> i == y &>> i)
    }
  }

  f(1 as UInt64, type: DWU64.self)
  f(~(~0 as UInt64 >> 1), type: DWU64.self)
  f(UInt64.max, type: DWU64.self)
  // 0b01010101_10100101_11110000_10100101_11110000_10100101_11110000_10100101
  f(17340530535757639845 as UInt64, type: DWU64.self)

  f(1 as Int64, type: DWI64.self)
  f(Int64.min, type: DWI64.self)
  f(Int64.max, type: DWI64.self)
  // 0b01010101_10100101_11110000_10100101_11110000_10100101_11110000_10100101
  f(6171603459878809765 as Int64, type: DWI64.self)
}

dwTests.test("Remainder/DividingBy0") {
  func f(_ x: Int1024, _ y: Int1024) -> Int1024 {
    return x % y
  }
  expectCrashLater()
  _ = f(42, 0)
}

dwTests.test("Division/By0") {
  func f(_ x: Int1024, _ y: Int1024) -> Int1024 {
    return x / y
  }
  expectCrashLater()
  _ = f(42, 0)
}

dwTests.test("DivideMinByMinusOne") {
  func f(_ x: Int1024) -> Int1024 {
    return x / -1
  }
  expectCrashLater()
  _ = f(Int1024.min)
}

dwTests.test("MultiplyMinByMinusOne") {
  func f(_ x: Int1024) -> Int1024 {
    return x * -1
  }
  expectCrashLater()
  _ = f(Int1024.min)
}

typealias DWI16 = DoubleWidth<Int8>
typealias DWU16 = DoubleWidth<UInt8>

dwTests.test("Conversions") {
  expectTrue(DWI16(1 << 15 - 1) == Int(1 << 15 - 1))
  expectTrue(DWI16(-1 << 15) == Int(-1 << 15))
  expectTrue(DWU16(1 << 16 - 1) == Int(1 << 16 - 1))
  expectTrue(DWU16(0) == Int(0))

  expectTrue(DWI16(Double(1 << 15 - 1)) == Int(1 << 15 - 1))
  expectTrue(DWI16(Double(-1 << 15)) == Int(-1 << 15))
  expectTrue(DWU16(Double(1 << 16 - 1)) == Int(1 << 16 - 1))
  expectTrue(DWU16(Double(0)) == Int(0))

  expectTrue(DWI16(Double(1 << 15 - 1) + 0.9) == Int(1 << 15 - 1))
  expectTrue(DWI16(Double(-1 << 15) - 0.9) == Int(-1 << 15))
  expectTrue(DWU16(Double(1 << 16 - 1) + 0.9) == Int(1 << 16 - 1))
  expectTrue(DWU16(Double(0) - 0.9) == Int(0))

  expectEqual(DWI16(0.00001), 0)
  expectEqual(DWU16(0.00001), 0)
}

dwTests.test("Exact Conversions") {
  expectEqual(DWI16(Double(1 << 15 - 1)), DWI16(exactly: Double(1 << 15 - 1))!)
  expectEqual(DWI16(Double(-1 << 15)), DWI16(exactly: Double(-1 << 15))!)
  expectEqual(DWU16(Double(1 << 16 - 1)), DWU16(exactly: Double(1 << 16 - 1))!)
  expectEqual(DWU16(Double(0)), DWU16(exactly: Double(0))!)

  expectNil(DWI16(exactly: Double(1 << 15 - 1) + 0.9))
  expectNil(DWI16(exactly: Double(-1 << 15) - 0.9))
  expectNil(DWU16(exactly: Double(1 << 16 - 1) + 0.9))
  expectNil(DWU16(exactly: Double(0) - 0.9))

  expectNil(DWI16(exactly: Double(1 << 15)))
  expectNil(DWI16(exactly: Double(-1 << 15) - 1))
  expectNil(DWU16(exactly: Double(1 << 16)))
  expectNil(DWU16(exactly: Double(-1)))

  expectNil(DWI16(exactly: 0.00001))
  expectNil(DWU16(exactly: 0.00001))

  expectNil(DWU16(exactly: Double.nan))
  expectNil(DWU16(exactly: Float.nan))
  expectNil(DWU16(exactly: Double.infinity))
  expectNil(DWU16(exactly: Float.infinity))
}

dwTests.test("Conversions/SignedMax+1") {
  expectCrashLater()
  _ = DWI16(1 << 15)
}

dwTests.test("Conversions/SignedMin-1") {
  expectCrashLater()
  _ = DWI16(-1 << 15 - 1)
}

dwTests.test("Conversions/UnsignedMax+1") {
  expectCrashLater()
  _ = DWU16(1 << 16)
}

dwTests.test("Conversions/Unsigned-1") {
  expectCrashLater()
  _ = DWU16(-1)
}

dwTests.test("Conversions/String") {
  expectEqual(String(Int256.max, radix: 16),
    "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
  expectEqual(String(Int256.min, radix: 16),
    "-8000000000000000000000000000000000000000000000000000000000000000")
  
  expectEqual(String(Int256.max, radix: 2), """
    1111111111111111111111111111111111111111111111111111111111111111\
    1111111111111111111111111111111111111111111111111111111111111111\
    1111111111111111111111111111111111111111111111111111111111111111\
    111111111111111111111111111111111111111111111111111111111111111
    """)
  expectEqual(String(Int256.min, radix: 2), """
    -100000000000000000000000000000000000000000000000000000000000000\
    0000000000000000000000000000000000000000000000000000000000000000\
    0000000000000000000000000000000000000000000000000000000000000000\
    00000000000000000000000000000000000000000000000000000000000000000
    """)
  
  expectEqual(String(Int128.max, radix: 10),
    "170141183460469231731687303715884105727")
  expectEqual(String(Int128.min, radix: 10),
    "-170141183460469231731687303715884105728")
}

dwTests.test("Words") {
  expectEqualSequence((0 as DoubleWidth<Int8>).words, [0])
  expectEqualSequence((1 as DoubleWidth<Int8>).words, [1])
  expectEqualSequence((-1 as DoubleWidth<Int8>).words, [UInt.max])
  expectEqualSequence((256 as DoubleWidth<Int8>).words, [256])
  expectEqualSequence((-256 as DoubleWidth<Int8>).words, [UInt.max - 255])
  expectEqualSequence(DoubleWidth<Int8>.max.words, [32767])
  expectEqualSequence(DoubleWidth<Int8>.min.words, [UInt.max - 32767])

  expectEqualSequence((0 as Int1024).words,
    repeatElement(0 as UInt, count: 1024 / UInt.bitWidth))
  expectEqualSequence((-1 as Int1024).words,
    repeatElement(UInt.max, count: 1024 / UInt.bitWidth))
  expectEqualSequence((1 as Int1024).words,
    [1] + Array(repeating: 0, count: 1024 / UInt.bitWidth - 1))
}

dwTests.test("Conditional Conformance") {
  checkSignedIntegerConformance(0 as Int128)
  checkSignedIntegerConformance(0 as Int1024)

  checkUnsignedIntegerConformance(0 as UInt128)
  checkUnsignedIntegerConformance(0 as UInt1024)
}

runAllTests()

