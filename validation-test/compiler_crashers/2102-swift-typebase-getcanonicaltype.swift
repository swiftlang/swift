// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol b {
protocol A : d {
typealias d
}
}
let h: [Any, A : b {
return
