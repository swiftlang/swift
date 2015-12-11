// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<T where T: A : b.Type) -> T : (n: b {
"""
enum A : CollectionType where S() -> {
}
typealias e = b(a)
