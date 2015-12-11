// RUN: not --crash %target-swift-frontend %s -c -o /dev/null
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/codafi (Robert Widmann)

struct X<T> {
	let s : X<X>
}
