// Constant globals on comparisons and conditions
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: rdar146953097 
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: Int = true ? 1 : 0
@const let constGlobal2: Int = (2 * 4 == 8) ? 1 : 0
@const let constGlobal3: Int = (10 > 20) ? 10 : 20
//@const let constGlobal4: Int = max(10, 20)
@const let constGlobal5: Bool = 10 > 20
@const let constGlobal6: Int = constGlobal5 ? 10 : 20

@const let number: UInt8 = 0xf0

@const let bit0: Bool = (number & 0b0000_0001) != 0
@const let bit1: Bool = (number & 0b0000_0010) != 0
@const let bit2: Bool = (number & 0b0000_0100) != 0
@const let bit3: Bool = (number & 0b0000_1000) != 0
@const let bit4: Bool = (number & 0b0001_0000) != 0
@const let bit5: Bool = (number & 0b0010_0000) != 0
@const let bit6: Bool = (number & 0b0100_0000) != 0
@const let bit7: Bool = (number & 0b1000_0000) != 0

@const let bits0_0: UInt8 =           ((bit0 ? 0b0000_0001 : 0b0000_0000) << 0)
@const let bits0_1: UInt8 = bits0_0 | ((bit1 ? 0b0000_0001 : 0b0000_0000) << 1)
@const let bits0_2: UInt8 = bits0_1 | ((bit2 ? 0b0000_0001 : 0b0000_0000) << 2)
@const let bits0_3: UInt8 = bits0_2 | ((bit3 ? 0b0000_0001 : 0b0000_0000) << 3)
@const let bits0_4: UInt8 = bits0_3 | ((bit4 ? 0b0000_0001 : 0b0000_0000) << 4)
@const let bits0_5: UInt8 = bits0_4 | ((bit5 ? 0b0000_0001 : 0b0000_0000) << 5)
@const let bits0_6: UInt8 = bits0_5 | ((bit6 ? 0b0000_0001 : 0b0000_0000) << 6)
@const let bits0_7: UInt8 = bits0_6 | ((bit7 ? 0b0000_0001 : 0b0000_0000) << 7)

@const let numberBackIsRight: Bool = bits0_7 == 0xf0
