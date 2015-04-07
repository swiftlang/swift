// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct l<e : SequenceType> {
    l g: e
}
func h<e>() -> [l<e>] {
    f []
}
func i(e: g) -> <j>(() -> j) -> k
