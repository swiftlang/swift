// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

// ASAN Output: stack-overflow on address 0x7ffdcd2b1fd0 (pc 0x0000008ecf9e bp 0x7ffdcd2b2810 sp 0x7ffdcd2b1fc0 T0)

enum A
protocol A{
typealias f:a
func a<T where f:d
