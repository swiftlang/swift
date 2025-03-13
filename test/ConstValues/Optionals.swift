// Constant globals on optionals
// REQUIRES: swift_feature_CompileTimeValues

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: Int? = 42
@const let constGlobal2: Int? = nil
@const let constGlobal3: UInt8? = 42
@const let constGlobal4: UInt8? = nil
@const let constGlobal5: Bool? = true
@const let constGlobal6: Bool? = nil
@const let constGlobal7: (Int, Int)? = (42, 42)
@const let constGlobal8: (Int, Int)? = nil
@const let constGlobal10: String? = nil
