// Constant globals on simple integer literals
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValues

@const let constGlobal1: Int = 42
@const let constGlobal2: UInt = 42
@const let constGlobal3: UInt = 0xf000f000
#if _pointerBitWidth(_64)
@const let constGlobal4: UInt = 0xf000f000_f000f000
#endif
@const let constGlobal5: UInt8 = 255
@const let constGlobal6: UInt32 = 0xffff_ffff
