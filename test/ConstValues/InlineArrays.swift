// Constant globals on inline arrays
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: rdar146954768
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -disable-availability-checking -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -disable-availability-checking -enable-experimental-feature CompileTimeValues

@const let constGlobal1: InlineArray = [1, 2, 3]
@const let constGlobal2: InlineArray = [1.0, 2.0, 3.0]
@const let constGlobal3: InlineArray = constGlobal1
@const let constGlobal4: Int = ([1, 2, 3] as InlineArray).count
