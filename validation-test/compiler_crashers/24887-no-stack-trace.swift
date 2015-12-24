// RUN: not --crash %target-swift-frontend %s -emit-silgen
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/codafi (Robert Widmann)

struct X<T> {
	let s : X<X>
}
