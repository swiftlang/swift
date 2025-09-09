// Constant globals on integer expressions
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// REQUIRES: optimized_stdlib
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview -verify

@const let constGlobal1: Int = (42 + 42 + 42) / 3
@const let constGlobal2: Int = MemoryLayout<UInt32>.size + 4
@const let constGlobal3: Int = Int(17.0 / 3.5)
@const let constGlobal4: Int = constGlobal1 + 1
@const let constGlobal5: Int = -constGlobal1 + 1
