// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

public var e: String {
return "[1]("
}
struct c {
protocol A {
typealias d: d where T
func a() {
