// Constant globals on comparisons and conditions

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int = true ? 1 : 0
_const let constGlobal2: Int = (2 * 4 == 8) ? 1 : 0
_const let constGlobal3: Int = (10 > 20) ? 10 : 20
//_const let constGlobal4: Int = max(10, 20)
_const let constGlobal5: Bool = 10 > 20
_const let constGlobal6: Int = constGlobal5 ? 10 : 20

_const let number: UInt8 = 0xf0

_const let bit0: Bool = (number & 0b0000_0001) != 0
_const let bit1: Bool = (number & 0b0000_0010) != 0
_const let bit2: Bool = (number & 0b0000_0100) != 0
_const let bit3: Bool = (number & 0b0000_1000) != 0
_const let bit4: Bool = (number & 0b0001_0000) != 0
_const let bit5: Bool = (number & 0b0010_0000) != 0
_const let bit6: Bool = (number & 0b0100_0000) != 0
_const let bit7: Bool = (number & 0b1000_0000) != 0

_const let bits0_0: UInt8 =           ((bit0 ? 0b0000_0001 : 0b0000_0000) << 0)
_const let bits0_1: UInt8 = bits0_0 | ((bit1 ? 0b0000_0001 : 0b0000_0000) << 1)
_const let bits0_2: UInt8 = bits0_1 | ((bit2 ? 0b0000_0001 : 0b0000_0000) << 2)
_const let bits0_3: UInt8 = bits0_2 | ((bit3 ? 0b0000_0001 : 0b0000_0000) << 3)
_const let bits0_4: UInt8 = bits0_3 | ((bit4 ? 0b0000_0001 : 0b0000_0000) << 4)
_const let bits0_5: UInt8 = bits0_4 | ((bit5 ? 0b0000_0001 : 0b0000_0000) << 5)
_const let bits0_6: UInt8 = bits0_5 | ((bit6 ? 0b0000_0001 : 0b0000_0000) << 6)
_const let bits0_7: UInt8 = bits0_6 | ((bit7 ? 0b0000_0001 : 0b0000_0000) << 7)

_const let numberBackIsRight: Bool = bits0_7 == 0xf0
