// Constant globals rejected for not being constant values
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview

@const let a: Bool = Bool.random()
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
// expected-error@-2 {{global variable must be a compile-time constant}} // Remove this once we common out the diagnostics

func foo() -> Int {
	return 42 * Int.random(in: 0 ..< 10)
}

@const let b: Int = foo()
// expected-error@-1 {{'@const' value should be initialized with a compile-time value}}
// expected-error@-2 {{global variable must be a compile-time constant}} // Remove this once we common out the diagnostics
