// Constant globals referencing other constant globals in their initializer expressions
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let a: Int = 42
@const let b: Int = a
@const let c: Int = b
