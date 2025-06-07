// Constant globals on tuples
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -verify

@const let constGlobal1: Int = 42
@const let constGlobal2: (Int, Int) = (42, 42)
@const let constGlobal3: (Int, Bool) = (42, true)
@const let constGlobal4: (Int, (Int, Int)) = (42, (42, 42))
@const let constGlobal5: (Int, Float) = (42, 42.0)

// Closure call not supported in syntactically-validated mode
@const let constGlobal7: (UInt64, StaticString, @convention(c) ()->Int) = (42, "hi", { return 42 }) // expected-error {{not supported in a '@const' expression}}
