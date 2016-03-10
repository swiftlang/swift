// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

public protocol P {}

public func foo<T, TargetStream : P>(
  value: T, inout _ target: TargetStream
) {
  fatalError()
}

{foo(""""?struct{
