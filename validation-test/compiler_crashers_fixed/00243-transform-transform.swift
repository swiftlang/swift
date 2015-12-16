// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18495979

struct A {
	func g<U>(h: (A, U) -> U) -> (A, U) -> U {
		return { _, x in return x }
	}
	func f() {
		let h: (A, A) -> A = { c, x in x }
		let b = g(h)(self, self)
	}
}
