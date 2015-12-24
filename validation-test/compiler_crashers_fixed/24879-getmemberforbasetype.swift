// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/zneak (zneak)

protocol A { typealias B }
class C : A { typealias B = Int }

func crash<D: C>() -> Bool {
	let a: D.B? = nil
}
