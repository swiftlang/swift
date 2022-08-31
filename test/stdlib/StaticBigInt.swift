//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift(-parse-as-library)
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// REQUIRES: reflection
// END.
//
//===----------------------------------------------------------------------===//

import StdlibUnittest

@available(SwiftStdlib 5.8, *)
@main
final class StaticBigIntTests {

  @available(SwiftStdlib 5.8, *)
  static func main() {
    let testCase = StaticBigIntTests()
    let testSuite = TestSuite("StaticBigIntTests")
    testSuite.test("BinaryRepresentation",  testCase.testBinaryRepresentation)
    testSuite.test("TextualRepresentation", testCase.testTextualRepresentation)
    testSuite.test("WrapperAssociatedType", testCase.testWrapperAssociatedType)
    runAllTests()
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Binary Representation
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.8, *)
extension StaticBigIntTests {

  @available(SwiftStdlib 5.8, *)
  func testBinaryRepresentation() {
    typealias Expected = (signum: Int, bitWidth: Int, words: [UInt])
    let m = UInt(bitPattern: .min)
    let keyValuePairs: KeyValuePairs<StaticBigInt, Expected> = [
      -0x80000000000000000000000000000002: (-1, 129, [~1, ~m, ~0]),
      -0x80000000000000000000000000000001: (-1, 129, [~0, ~m, ~0]),
      -0x80000000000000000000000000000000: (-1, 128, [ 0,  m, ~0]),
      -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF: (-1, 128, [ 1,  m, ~0]),
      -0x4:                                (-1,   3, [~3, ~0, ~0]),
      -0x3:                                (-1,   3, [~2, ~0, ~0]),
      -0x2:                                (-1,   2, [~1, ~0, ~0]),
      -0x1:                                (-1,   1, [~0, ~0, ~0]),
      +0x0:                                ( 0,   1, [ 0,  0,  0]),
      +0x1:                                (+1,   2, [ 1,  0,  0]),
      +0x2:                                (+1,   3, [ 2,  0,  0]),
      +0x3:                                (+1,   3, [ 3,  0,  0]),
      +0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE: (+1, 128, [~1, ~m,  0]),
      +0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF: (+1, 128, [~0, ~m,  0]),
      +0x80000000000000000000000000000000: (+1, 129, [ 0,  m,  0]),
      +0x80000000000000000000000000000001: (+1, 129, [ 1,  m,  0]),
    ]
    for (actual, expected) in keyValuePairs {
      expectEqual(expected.signum,   actual.signum())
      expectEqual(expected.bitWidth, actual.bitWidth)
      expectEqual(expected.words[0], actual[0])
      expectEqual(expected.words[1], actual[1])
      expectEqual(expected.words[2], actual[2])
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Textual Representation
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.8, *)
extension StaticBigIntTests {

  @available(SwiftStdlib 5.8, *)
  func testTextualRepresentation() {
    let keyValuePairs: KeyValuePairs<StaticBigInt, String> = [
      -0x80000000000000000000000000000002: "-0x80000000000000000000000000000002",
      -0x80000000000000000000000000000001: "-0x80000000000000000000000000000001",
      -0x80000000000000000000000000000000: "-0x80000000000000000000000000000000",
      -0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF: "-0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      -0x123456789ABCDE:                   "-0x123456789ABCDE",
      -0x123456789ABCD:                    "-0x123456789ABCD",
      -0x123456789ABC:                     "-0x123456789ABC",
      -0x1000:                             "-0x1000",
      -0x100:                              "-0x100",
      -0x10:                               "-0x10",
      -0x4:                                "-0x4",
      -0x3:                                "-0x3",
      -0x2:                                "-0x2",
      -0x1:                                "-0x1",
      +0x0:                                "+0x0",
      +0x1:                                "+0x1",
      +0x2:                                "+0x2",
      +0x3:                                "+0x3",
      +0x10:                               "+0x10",
      +0x100:                              "+0x100",
      +0x1000:                             "+0x1000",
      +0x123456789ABC:                     "+0x123456789ABC",
      +0x123456789ABCD:                    "+0x123456789ABCD",
      +0x123456789ABCDE:                   "+0x123456789ABCDE",
      +0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE: "+0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE",
      +0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF: "+0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
      +0x80000000000000000000000000000000: "+0x80000000000000000000000000000000",
      +0x80000000000000000000000000000001: "+0x80000000000000000000000000000001",
    ]
    for (actual, expected) in keyValuePairs {
      expectEqual(expected, String(reflecting: actual))
    }
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Wrapper Associated Type
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.8, *)
extension StaticBigIntTests {

  @available(SwiftStdlib 5.8, *)
  struct Wrapper: ExpressibleByIntegerLiteral {

    @available(SwiftStdlib 5.8, *)
    let actual: StaticBigInt

    @available(SwiftStdlib 5.8, *)
    init(integerLiteral actual: StaticBigInt) {
      self.actual = actual
    }
  }

  @available(SwiftStdlib 5.8, *)
  func testWrapperAssociatedType() {
    do {
      let negative = Wrapper(-0x0011223344556677_8899AABBCCDDEEFF)
      expectEqual( -1, negative.actual.signum())
      expectEqual(118, negative.actual.bitWidth)
      expectEqual(0x7766554433221101, negative.actual[0])
      expectEqual(0xFFEEDDCCBBAA9988, negative.actual[1])
      expectEqual(0xFFFFFFFFFFFFFFFF, negative.actual[2])
      expectEqual(0xFFFFFFFFFFFFFFFF, negative.actual[.max])
    }
    do {
      let positive = Wrapper(0x0011223344556677_8899AABBCCDDEEFF)
      expectEqual( +1, positive.actual.signum())
      expectEqual(118, positive.actual.bitWidth)
      expectEqual(0x8899AABBCCDDEEFF, positive.actual[0])
      expectEqual(0x0011223344556677, positive.actual[1])
      expectEqual(0x0000000000000000, positive.actual[2])
      expectEqual(0x0000000000000000, positive.actual[.max])
    }
    do {
      let fibonacciSequence = Wrapper(
        0xA94FAD42221F2702_68A3DD8E61ECCFBD_40ABCFB3C0325745_27F80DDAA1BA7878_18B3C1D91E77DECD_0F444C01834299AB_096F75D79B354522_05D4D629E80D5489_039A9FADB327F099_023A367C34E563F0_016069317E428CA9_00D9CD4AB6A2D747_00869BE6C79FB562_00533163EF0321E5_00336A82D89C937D_001FC6E116668E68_0013A3A1C2360515_000C233F54308953_000780626E057BC2_0004A2DCE62B0D91_0002DD8587DA6E31_0001C5575E509F60_0001182E2989CED1_0000AD2934C6D08F_00006B04F4C2FE42_000042244003D24D_000028E0B4BF2BF5_000019438B44A658_00000F9D297A859D_000009A661CA20BB_000005F6C7B064E2_000003AF9A19BBD9_000002472D96A909_000001686C8312D0_000000DEC1139639_00000089AB6F7C97_0000005515A419A2_0000003495CB62F5_000000207FD8B6AD_0000001415F2AC48_0000000C69E60A65_00000007AC0CA1E3_00000004BDD96882_00000002EE333961_00000001CFA62F21_000000011E8D0A40_00000000B11924E1_000000006D73E55F_0000000043A53F82_0000000029CEA5DD_0000000019D699A5_000000000FF80C38_0000000009DE8D6D_0000000006197ECB_0000000003C50EA2_0000000002547029_0000000001709E79_0000000000E3D1B0_00000000008CCCC9_00000000005704E7_000000000035C7E2_0000000000213D05_0000000000148ADD_00000000000CB228_000000000007D8B5_000000000004D973_000000000002FF42_000000000001DA31_0000000000012511_000000000000B520_0000000000006FF1_000000000000452F_0000000000002AC2_0000000000001A6D_0000000000001055_0000000000000A18_000000000000063D_00000000000003DB_0000000000000262_0000000000000179_00000000000000E9_0000000000000090_0000000000000059_0000000000000037_0000000000000022_0000000000000015_000000000000000D_0000000000000008_0000000000000005_0000000000000003_0000000000000002_0000000000000001_0000000000000001_0000000000000000
      )
      expectEqual(
        1 + (94 * UInt.bitWidth), //-> 6017 bits.
        fibonacciSequence.actual.bitWidth
      )
      for wordIndex in 2..<94 {
        expectEqual(
          fibonacciSequence.actual[wordIndex],
          fibonacciSequence.actual[wordIndex - 1] +
          fibonacciSequence.actual[wordIndex - 2]
        )
      }
    }
  }
}
