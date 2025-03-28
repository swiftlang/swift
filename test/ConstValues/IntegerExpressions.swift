// Constant globals on integer expressions
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -verify

@const let constGlobal1: Int = (42 + 42 + 42) / 3
@const let constGlobal2: Int = MemoryLayout<UInt32>.size + 4
@const let constGlobal3: Int = Int(17.0 / 3.5)
// expected-error@-1 {{@const value should be initialized with a compile-time value}}
// expected-error@-2 {{global variable must be a compile-time constant}} // Remove this once we common out the diagnostics
@const let constGlobal4: Int = constGlobal1 + 1
@const let constGlobal5: Int = -constGlobal1 + 1
