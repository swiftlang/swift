// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
class a {
}
struct c<S: SequenceType, T where Optional<T> == S.Generator.Element>(
