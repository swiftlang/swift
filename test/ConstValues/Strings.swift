// Constant globals on strings
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: String = "hello"
@const let constGlobal2: StaticString = "hello"
@const let constGlobal3: Int = "hello".count
