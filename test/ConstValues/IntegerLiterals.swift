// Constant globals on simple integer literals

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

_const let constGlobal1: Int = 42
_const let constGlobal2: UInt = 42
_const let constGlobal3: UInt = 0xf000f000
#if _pointerBitWidth(_64)
_const let constGlobal4: UInt = 0xf000f000_f000f000
#endif
_const let constGlobal5: UInt8 = 255
_const let constGlobal6: UInt32 = 0xffff_ffff
