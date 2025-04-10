// Constant globals should "work" even in top-level code mode.
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: rdar146954355
// RUN: %target-swift-frontend -emit-ir -primary-file %s -enable-experimental-feature CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues


@const let constGlobal: Int = 42
