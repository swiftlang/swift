// Constant globals on integer arithmetics
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: Int = 42
@const let constGlobal2: UInt = 42 + 42
@const let constGlobal3: UInt = 42 * 42
@const let constGlobal4: UInt = 42 - 42
@const let constGlobal5: UInt = 42 / 42
@const let constGlobal6: UInt = 42 % 2
@const let constGlobal7: UInt = (42 % 2)
