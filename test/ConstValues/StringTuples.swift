// Constant globals on tuples
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: rdar146953330
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobalStringTuple: (Int, String) = (42, "Foo")
