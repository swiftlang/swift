// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class c {
func b((Any, c))(Any, AnyObject
}
struct A<T> {
}
struct c<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs
