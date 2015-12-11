// RUN: not %target-swift-frontend %s -parse


// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct c<d : SequenceType> {
var b:  [c<d>] {
return []
}
protocol a {
}
protocol d : b { func b
