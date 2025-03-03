// Constant globals rejected for not being constant values

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify

_const let a: Bool = Bool.random()
// expected-error@-1 {{_const let should be initialized with a compile-time value}}

func foo() -> Int {
	return 42 * Int.random(in: 0 ..< 10)
}

_const let b: Int = foo()
// expected-error@-1 {{_const let should be initialized with a compile-time value}}
