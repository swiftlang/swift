// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

struct c<d : SequenceType> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
