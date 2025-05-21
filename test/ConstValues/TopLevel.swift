// Constant globals should "work" even in top-level code mode.
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: rdar146954355
// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview

// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal: Int = 42
