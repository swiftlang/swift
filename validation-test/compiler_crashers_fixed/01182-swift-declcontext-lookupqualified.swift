// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class i: d, c f
struct c<d: Sequence, b where Optional<b> == d.Iterator.Element
