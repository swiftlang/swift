// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

extension Array {
struct X<Element : A: (s: Element, Bool]
return ")
protocol A : X<Element>
