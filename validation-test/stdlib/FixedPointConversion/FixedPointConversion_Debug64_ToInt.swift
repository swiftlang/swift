//===----------------------------------------------------------------------===//
//
// Automatically Generated From ./Inputs/FixedPointConversion.swift.gyb
// Do Not Edit Directly!
//
//===----------------------------------------------------------------------===//
//
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// RUN: %target-run-simple-swift(-Onone)
// END.
//
//===----------------------------------------------------------------------===//

import StdlibUnittest

let FixedPointConversion_Debug64_ToInt = TestSuite("FixedPointConversion_Debug64_ToInt")

//===----------------------------------------------------------------------===//
// MARK: UInt8: (0)...(255)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromUInt8(0)_NeverTraps") {
  let input = getUInt8(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt8(0)_NeverFails") {
  let input = getUInt8(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt8(255)_NeverTraps") {
  let input = getUInt8(255)
  let actual = Int(input)
  expectEqual(255, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt8(255)_NeverFails") {
  let input = getUInt8(255)
  let actual = Int(exactly: input)
  expectEqual(255, actual)
}

//===----------------------------------------------------------------------===//
// MARK: Int8: (-128)...(127)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromInt8(-128)_NeverTraps") {
  let input = getInt8(-128)
  let actual = Int(input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt8(-128)_NeverFails") {
  let input = getInt8(-128)
  let actual = Int(exactly: input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt8(0)_NeverTraps") {
  let input = getInt8(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt8(0)_NeverFails") {
  let input = getInt8(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt8(127)_NeverTraps") {
  let input = getInt8(127)
  let actual = Int(input)
  expectEqual(127, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt8(127)_NeverFails") {
  let input = getInt8(127)
  let actual = Int(exactly: input)
  expectEqual(127, actual)
}

//===----------------------------------------------------------------------===//
// MARK: UInt16: (0)...(65535)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromUInt16(0)_NeverTraps") {
  let input = getUInt16(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt16(0)_NeverFails") {
  let input = getUInt16(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt16(65535)_NeverTraps") {
  let input = getUInt16(65535)
  let actual = Int(input)
  expectEqual(65535, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt16(65535)_NeverFails") {
  let input = getUInt16(65535)
  let actual = Int(exactly: input)
  expectEqual(65535, actual)
}

//===----------------------------------------------------------------------===//
// MARK: Int16: (-32768)...(32767)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromInt16(-32768)_NeverTraps") {
  let input = getInt16(-32768)
  let actual = Int(input)
  expectEqual(-32768, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt16(-32768)_NeverFails") {
  let input = getInt16(-32768)
  let actual = Int(exactly: input)
  expectEqual(-32768, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt16(0)_NeverTraps") {
  let input = getInt16(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt16(0)_NeverFails") {
  let input = getInt16(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt16(32767)_NeverTraps") {
  let input = getInt16(32767)
  let actual = Int(input)
  expectEqual(32767, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt16(32767)_NeverFails") {
  let input = getInt16(32767)
  let actual = Int(exactly: input)
  expectEqual(32767, actual)
}

//===----------------------------------------------------------------------===//
// MARK: UInt32: (0)...(4294967295)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromUInt32(0)_NeverTraps") {
  let input = getUInt32(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt32(0)_NeverFails") {
  let input = getUInt32(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt32(4294967295)_NeverTraps") {
  let input = getUInt32(4294967295)
  let actual = Int(input)
  expectEqual(4294967295, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt32(4294967295)_NeverFails") {
  let input = getUInt32(4294967295)
  let actual = Int(exactly: input)
  expectEqual(4294967295, actual)
}

//===----------------------------------------------------------------------===//
// MARK: Int32: (-2147483648)...(2147483647)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromInt32(-2147483648)_NeverTraps") {
  let input = getInt32(-2147483648)
  let actual = Int(input)
  expectEqual(-2147483648, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt32(-2147483648)_NeverFails") {
  let input = getInt32(-2147483648)
  let actual = Int(exactly: input)
  expectEqual(-2147483648, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt32(0)_NeverTraps") {
  let input = getInt32(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt32(0)_NeverFails") {
  let input = getInt32(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt32(2147483647)_NeverTraps") {
  let input = getInt32(2147483647)
  let actual = Int(input)
  expectEqual(2147483647, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt32(2147483647)_NeverFails") {
  let input = getInt32(2147483647)
  let actual = Int(exactly: input)
  expectEqual(2147483647, actual)
}

//===----------------------------------------------------------------------===//
// MARK: UInt64: (0)...(18446744073709551615)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromUInt64(0)_NeverTraps") {
  let input = getUInt64(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(0)_NeverFails") {
  let input = getUInt64(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(9223372036854775807)_NeverTraps") {
  let input = getUInt64(9223372036854775807)
  let actual = Int(input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(9223372036854775807)_NeverFails") {
  let input = getUInt64(9223372036854775807)
  let actual = Int(exactly: input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(9223372036854775808)_AlwaysTraps") {
  let input = getUInt64(9223372036854775808)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(9223372036854775808)_AlwaysFails") {
  let input = getUInt64(9223372036854775808)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(18446744073709551615)_AlwaysTraps") {
  let input = getUInt64(18446744073709551615)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromUInt64(18446744073709551615)_AlwaysFails") {
  let input = getUInt64(18446744073709551615)
  let actual = Int(exactly: input)
  expectNil(actual)
}

//===----------------------------------------------------------------------===//
// MARK: Int64: (-9223372036854775808)...(9223372036854775807)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromInt64(-9223372036854775808)_NeverTraps") {
  let input = getInt64(-9223372036854775808)
  let actual = Int(input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt64(-9223372036854775808)_NeverFails") {
  let input = getInt64(-9223372036854775808)
  let actual = Int(exactly: input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt64(0)_NeverTraps") {
  let input = getInt64(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt64(0)_NeverFails") {
  let input = getInt64(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt64(9223372036854775807)_NeverTraps") {
  let input = getInt64(9223372036854775807)
  let actual = Int(input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt64(9223372036854775807)_NeverFails") {
  let input = getInt64(9223372036854775807)
  let actual = Int(exactly: input)
  expectEqual(9223372036854775807, actual)
}

//===----------------------------------------------------------------------===//
// MARK: UInt: (0)...(18446744073709551615)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromUInt(0)_NeverTraps") {
  let input = getUInt(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(0)_NeverFails") {
  let input = getUInt(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(9223372036854775807)_NeverTraps") {
  let input = getUInt(9223372036854775807)
  let actual = Int(input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(9223372036854775807)_NeverFails") {
  let input = getUInt(9223372036854775807)
  let actual = Int(exactly: input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(9223372036854775808)_AlwaysTraps") {
  let input = getUInt(9223372036854775808)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(9223372036854775808)_AlwaysFails") {
  let input = getUInt(9223372036854775808)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(18446744073709551615)_AlwaysTraps") {
  let input = getUInt(18446744073709551615)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromUInt(18446744073709551615)_AlwaysFails") {
  let input = getUInt(18446744073709551615)
  let actual = Int(exactly: input)
  expectNil(actual)
}

//===----------------------------------------------------------------------===//
// MARK: Int: (-9223372036854775808)...(9223372036854775807)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromInt(-9223372036854775808)_NeverTraps") {
  let input = getInt(-9223372036854775808)
  let actual = Int(input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt(-9223372036854775808)_NeverFails") {
  let input = getInt(-9223372036854775808)
  let actual = Int(exactly: input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt(0)_NeverTraps") {
  let input = getInt(0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt(0)_NeverFails") {
  let input = getInt(0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt(9223372036854775807)_NeverTraps") {
  let input = getInt(9223372036854775807)
  let actual = Int(input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromInt(9223372036854775807)_NeverFails") {
  let input = getInt(9223372036854775807)
  let actual = Int(exactly: input)
  expectEqual(9223372036854775807, actual)
}

//===----------------------------------------------------------------------===//
// MARK: Float16: (-2047)...(2047)
//===----------------------------------------------------------------------===//

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
if #available(macOS 11.0, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-2047)_NeverTraps") {
  let input = getFloat16(-2047)
  let actual = Int(input)
  expectEqual(-2047, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-2047)_NeverFails") {
  let input = getFloat16(-2047)
  let actual = Int(exactly: input)
  expectEqual(-2047, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-128.5)_NeverTraps") {
  let input = getFloat16(-128.5)
  let actual = Int(input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-128.5)_AlwaysFails") {
  let input = getFloat16(-128.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-0.5)_NeverTraps") {
  let input = getFloat16(-0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-0.5)_AlwaysFails") {
  let input = getFloat16(-0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-0.0)_NeverTraps") {
  let input = getFloat16(-0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-0.0)_NeverFails") {
  let input = getFloat16(-0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(0.0)_NeverTraps") {
  let input = getFloat16(0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(0.0)_NeverFails") {
  let input = getFloat16(0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(0.5)_NeverTraps") {
  let input = getFloat16(0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(0.5)_AlwaysFails") {
  let input = getFloat16(0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(127.5)_NeverTraps") {
  let input = getFloat16(127.5)
  let actual = Int(input)
  expectEqual(127, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(127.5)_AlwaysFails") {
  let input = getFloat16(127.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(255.5)_NeverTraps") {
  let input = getFloat16(255.5)
  let actual = Int(input)
  expectEqual(255, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(255.5)_AlwaysFails") {
  let input = getFloat16(255.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(2047)_NeverTraps") {
  let input = getFloat16(2047)
  let actual = Int(input)
  expectEqual(2047, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(2047)_NeverFails") {
  let input = getFloat16(2047)
  let actual = Int(exactly: input)
  expectEqual(2047, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.infinity)_AlwaysTraps") {
  let input = getFloat16(-.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.infinity)_AlwaysFails") {
  let input = getFloat16(-.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.infinity)_AlwaysTraps") {
  let input = getFloat16(+.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.infinity)_AlwaysFails") {
  let input = getFloat16(+.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.nan)_AlwaysTraps") {
  let input = getFloat16(-.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.nan)_AlwaysFails") {
  let input = getFloat16(-.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.nan)_AlwaysTraps") {
  let input = getFloat16(+.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.nan)_AlwaysFails") {
  let input = getFloat16(+.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.signalingNaN)_AlwaysTraps") {
  let input = getFloat16(-.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(-.signalingNaN)_AlwaysFails") {
  let input = getFloat16(-.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.signalingNaN)_AlwaysTraps") {
  let input = getFloat16(+.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat16(+.signalingNaN)_AlwaysFails") {
  let input = getFloat16(+.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

}
#endif // Float16

//===----------------------------------------------------------------------===//
// MARK: Float32: (-16777215)...(16777215)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-16777215)_NeverTraps") {
  let input = getFloat32(-16777215)
  let actual = Int(input)
  expectEqual(-16777215, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-16777215)_NeverFails") {
  let input = getFloat32(-16777215)
  let actual = Int(exactly: input)
  expectEqual(-16777215, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-32768.5)_NeverTraps") {
  let input = getFloat32(-32768.5)
  let actual = Int(input)
  expectEqual(-32768, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-32768.5)_AlwaysFails") {
  let input = getFloat32(-32768.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-128.5)_NeverTraps") {
  let input = getFloat32(-128.5)
  let actual = Int(input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-128.5)_AlwaysFails") {
  let input = getFloat32(-128.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-0.5)_NeverTraps") {
  let input = getFloat32(-0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-0.5)_AlwaysFails") {
  let input = getFloat32(-0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-0.0)_NeverTraps") {
  let input = getFloat32(-0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-0.0)_NeverFails") {
  let input = getFloat32(-0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(0.0)_NeverTraps") {
  let input = getFloat32(0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(0.0)_NeverFails") {
  let input = getFloat32(0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(0.5)_NeverTraps") {
  let input = getFloat32(0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(0.5)_AlwaysFails") {
  let input = getFloat32(0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(127.5)_NeverTraps") {
  let input = getFloat32(127.5)
  let actual = Int(input)
  expectEqual(127, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(127.5)_AlwaysFails") {
  let input = getFloat32(127.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(255.5)_NeverTraps") {
  let input = getFloat32(255.5)
  let actual = Int(input)
  expectEqual(255, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(255.5)_AlwaysFails") {
  let input = getFloat32(255.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(32767.5)_NeverTraps") {
  let input = getFloat32(32767.5)
  let actual = Int(input)
  expectEqual(32767, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(32767.5)_AlwaysFails") {
  let input = getFloat32(32767.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(65535.5)_NeverTraps") {
  let input = getFloat32(65535.5)
  let actual = Int(input)
  expectEqual(65535, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(65535.5)_AlwaysFails") {
  let input = getFloat32(65535.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(16777215)_NeverTraps") {
  let input = getFloat32(16777215)
  let actual = Int(input)
  expectEqual(16777215, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(16777215)_NeverFails") {
  let input = getFloat32(16777215)
  let actual = Int(exactly: input)
  expectEqual(16777215, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.infinity)_AlwaysTraps") {
  let input = getFloat32(-.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.infinity)_AlwaysFails") {
  let input = getFloat32(-.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.infinity)_AlwaysTraps") {
  let input = getFloat32(+.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.infinity)_AlwaysFails") {
  let input = getFloat32(+.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.nan)_AlwaysTraps") {
  let input = getFloat32(-.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.nan)_AlwaysFails") {
  let input = getFloat32(-.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.nan)_AlwaysTraps") {
  let input = getFloat32(+.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.nan)_AlwaysFails") {
  let input = getFloat32(+.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.signalingNaN)_AlwaysTraps") {
  let input = getFloat32(-.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(-.signalingNaN)_AlwaysFails") {
  let input = getFloat32(-.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.signalingNaN)_AlwaysTraps") {
  let input = getFloat32(+.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat32(+.signalingNaN)_AlwaysFails") {
  let input = getFloat32(+.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

//===----------------------------------------------------------------------===//
// MARK: Float64: (-9007199254740991)...(9007199254740991)
//===----------------------------------------------------------------------===//

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-9007199254740991)_NeverTraps") {
  let input = getFloat64(-9007199254740991)
  let actual = Int(input)
  expectEqual(-9007199254740991, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-9007199254740991)_NeverFails") {
  let input = getFloat64(-9007199254740991)
  let actual = Int(exactly: input)
  expectEqual(-9007199254740991, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-32768.5)_NeverTraps") {
  let input = getFloat64(-32768.5)
  let actual = Int(input)
  expectEqual(-32768, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-32768.5)_AlwaysFails") {
  let input = getFloat64(-32768.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-128.5)_NeverTraps") {
  let input = getFloat64(-128.5)
  let actual = Int(input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-128.5)_AlwaysFails") {
  let input = getFloat64(-128.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-0.5)_NeverTraps") {
  let input = getFloat64(-0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-0.5)_AlwaysFails") {
  let input = getFloat64(-0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-0.0)_NeverTraps") {
  let input = getFloat64(-0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-0.0)_NeverFails") {
  let input = getFloat64(-0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(0.0)_NeverTraps") {
  let input = getFloat64(0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(0.0)_NeverFails") {
  let input = getFloat64(0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(0.5)_NeverTraps") {
  let input = getFloat64(0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(0.5)_AlwaysFails") {
  let input = getFloat64(0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(127.5)_NeverTraps") {
  let input = getFloat64(127.5)
  let actual = Int(input)
  expectEqual(127, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(127.5)_AlwaysFails") {
  let input = getFloat64(127.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(255.5)_NeverTraps") {
  let input = getFloat64(255.5)
  let actual = Int(input)
  expectEqual(255, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(255.5)_AlwaysFails") {
  let input = getFloat64(255.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(32767.5)_NeverTraps") {
  let input = getFloat64(32767.5)
  let actual = Int(input)
  expectEqual(32767, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(32767.5)_AlwaysFails") {
  let input = getFloat64(32767.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(65535.5)_NeverTraps") {
  let input = getFloat64(65535.5)
  let actual = Int(input)
  expectEqual(65535, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(65535.5)_AlwaysFails") {
  let input = getFloat64(65535.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(9007199254740991)_NeverTraps") {
  let input = getFloat64(9007199254740991)
  let actual = Int(input)
  expectEqual(9007199254740991, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(9007199254740991)_NeverFails") {
  let input = getFloat64(9007199254740991)
  let actual = Int(exactly: input)
  expectEqual(9007199254740991, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.infinity)_AlwaysTraps") {
  let input = getFloat64(-.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.infinity)_AlwaysFails") {
  let input = getFloat64(-.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.infinity)_AlwaysTraps") {
  let input = getFloat64(+.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.infinity)_AlwaysFails") {
  let input = getFloat64(+.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.nan)_AlwaysTraps") {
  let input = getFloat64(-.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.nan)_AlwaysFails") {
  let input = getFloat64(-.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.nan)_AlwaysTraps") {
  let input = getFloat64(+.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.nan)_AlwaysFails") {
  let input = getFloat64(+.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.signalingNaN)_AlwaysTraps") {
  let input = getFloat64(-.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(-.signalingNaN)_AlwaysFails") {
  let input = getFloat64(-.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.signalingNaN)_AlwaysTraps") {
  let input = getFloat64(+.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat64(+.signalingNaN)_AlwaysFails") {
  let input = getFloat64(+.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

//===----------------------------------------------------------------------===//
// MARK: Float80: (-18446744073709551615)...(18446744073709551615)
//===----------------------------------------------------------------------===//

#if !(os(Windows) || os(Android)) && (arch(i386) || arch(x86_64))

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-18446744073709551615)_AlwaysTraps") {
  let input = getFloat80(-18446744073709551615)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-18446744073709551615)_AlwaysFails") {
  let input = getFloat80(-18446744073709551615)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-9223372036854775809)_AlwaysTraps") {
  let input = getFloat80(-9223372036854775809)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-9223372036854775809)_AlwaysFails") {
  let input = getFloat80(-9223372036854775809)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-9223372036854775808)_NeverTraps") {
  let input = getFloat80(-9223372036854775808)
  let actual = Int(input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-9223372036854775808)_NeverFails") {
  let input = getFloat80(-9223372036854775808)
  let actual = Int(exactly: input)
  expectEqual(-9223372036854775808, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-32768.5)_NeverTraps") {
  let input = getFloat80(-32768.5)
  let actual = Int(input)
  expectEqual(-32768, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-32768.5)_AlwaysFails") {
  let input = getFloat80(-32768.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-128.5)_NeverTraps") {
  let input = getFloat80(-128.5)
  let actual = Int(input)
  expectEqual(-128, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-128.5)_AlwaysFails") {
  let input = getFloat80(-128.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-0.5)_NeverTraps") {
  let input = getFloat80(-0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-0.5)_AlwaysFails") {
  let input = getFloat80(-0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-0.0)_NeverTraps") {
  let input = getFloat80(-0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-0.0)_NeverFails") {
  let input = getFloat80(-0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(0.0)_NeverTraps") {
  let input = getFloat80(0.0)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(0.0)_NeverFails") {
  let input = getFloat80(0.0)
  let actual = Int(exactly: input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(0.5)_NeverTraps") {
  let input = getFloat80(0.5)
  let actual = Int(input)
  expectEqual(0, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(0.5)_AlwaysFails") {
  let input = getFloat80(0.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(127.5)_NeverTraps") {
  let input = getFloat80(127.5)
  let actual = Int(input)
  expectEqual(127, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(127.5)_AlwaysFails") {
  let input = getFloat80(127.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(255.5)_NeverTraps") {
  let input = getFloat80(255.5)
  let actual = Int(input)
  expectEqual(255, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(255.5)_AlwaysFails") {
  let input = getFloat80(255.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(32767.5)_NeverTraps") {
  let input = getFloat80(32767.5)
  let actual = Int(input)
  expectEqual(32767, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(32767.5)_AlwaysFails") {
  let input = getFloat80(32767.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(65535.5)_NeverTraps") {
  let input = getFloat80(65535.5)
  let actual = Int(input)
  expectEqual(65535, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(65535.5)_AlwaysFails") {
  let input = getFloat80(65535.5)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(9223372036854775807)_NeverTraps") {
  let input = getFloat80(9223372036854775807)
  let actual = Int(input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(9223372036854775807)_NeverFails") {
  let input = getFloat80(9223372036854775807)
  let actual = Int(exactly: input)
  expectEqual(9223372036854775807, actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(9223372036854775808)_AlwaysTraps") {
  let input = getFloat80(9223372036854775808)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(9223372036854775808)_AlwaysFails") {
  let input = getFloat80(9223372036854775808)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(18446744073709551615)_AlwaysTraps") {
  let input = getFloat80(18446744073709551615)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(18446744073709551615)_AlwaysFails") {
  let input = getFloat80(18446744073709551615)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.infinity)_AlwaysTraps") {
  let input = getFloat80(-.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.infinity)_AlwaysFails") {
  let input = getFloat80(-.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.infinity)_AlwaysTraps") {
  let input = getFloat80(+.infinity)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.infinity)_AlwaysFails") {
  let input = getFloat80(+.infinity)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.nan)_AlwaysTraps") {
  let input = getFloat80(-.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.nan)_AlwaysFails") {
  let input = getFloat80(-.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.nan)_AlwaysTraps") {
  let input = getFloat80(+.nan)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.nan)_AlwaysFails") {
  let input = getFloat80(+.nan)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.signalingNaN)_AlwaysTraps") {
  let input = getFloat80(-.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(-.signalingNaN)_AlwaysFails") {
  let input = getFloat80(-.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.signalingNaN)_AlwaysTraps") {
  let input = getFloat80(+.signalingNaN)
  expectCrash {
    let actual = Int(input)
    _blackHole(actual)
  }
}

FixedPointConversion_Debug64_ToInt.test("FromFloat80(+.signalingNaN)_AlwaysFails") {
  let input = getFloat80(+.signalingNaN)
  let actual = Int(exactly: input)
  expectNil(actual)
}

#endif // Float80

runAllTests()

