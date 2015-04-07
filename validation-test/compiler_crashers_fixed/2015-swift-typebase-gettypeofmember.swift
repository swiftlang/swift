// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
typealias B : I.join(self.init(A, U, "))->()
}
extension A {
enum B == c> {
}
}
class B : A,
