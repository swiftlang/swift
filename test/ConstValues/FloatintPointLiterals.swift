// Constant globals on simple floating-point literals
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1 = 42.0
@const let constGlobal2: Double = 42.0
@const let constGlobal3: Float = 42.0

// TODO: Restrict to platforms where this is available
//@const let constGlobal4: Float16 = 42.0
