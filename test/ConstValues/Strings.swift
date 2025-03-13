// Constant globals on strings
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: rdar146953748
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: String = "hello"
@const let constGlobal2: StaticString = "hello"
@const let constGlobal3: Int = "hello".count
