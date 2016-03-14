// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts
public protocol P {}
public func foo<T, TargetStream : P>(
  value: T, inout _ target: TargetStream
) {
  fatalError()
}
{foo(""""?struct{
