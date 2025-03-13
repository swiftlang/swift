// Constant globals on tuples
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues

@const let constGlobal1: Int = 42
@const let constGlobal2: (Int, Int) = (42, 42)
@const let constGlobal3: (Int, Bool) = (42, true)
@const let constGlobal4: (Int, (Int, Int)) = (42, (42, 42))
@const let constGlobal5: (Int, Float) = (42, 42.0)
@const let constGlobal7: (UInt64, StaticString, @convention(c) ()->Int) = (42, "hi", { return 42 })
