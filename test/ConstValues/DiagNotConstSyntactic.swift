// Constant globals rejected for not being constant values
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify -enable-experimental-feature CompileTimeValues

@const let a: Bool = Bool.random()
// expected-error@-1 {{not supported in a '@const' expression}}

func foo() -> Int {
	return 42 * Int.random(in: 0 ..< 10)
}

@const let b: Int = foo()
// expected-error@-1 {{not supported in a '@const' expression}}
