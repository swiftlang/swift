// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -strict-concurrency=targeted
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify %s -verify-additional-prefix complete-and-tns- -strict-concurrency=complete

// REQUIRES: concurrency

// https://github.com/apple/swift/issues/59909
struct Future<T> { }

extension Future {
  @preconcurrency
  func flatMap<NewValue>(_ callback: @escaping @Sendable (T) -> Future<NewValue>) -> Future<NewValue> { // #1
    fatalError()
  }

  @preconcurrency
  public func flatMapErrorThrowing(_ callback: @escaping @Sendable (Error) throws -> T) -> Future<T> {
    fatalError("")
  }
}

extension Future {
  @available(*, deprecated, message: "")
  func flatMap<NewValue>(file: StaticString = #file, line: UInt = #line, _ callback: @escaping (T) -> Future<NewValue>) -> Future<NewValue> { // #2
    // expected-complete-and-tns-note @-1 {{parameter 'callback' is implicitly non-Sendable}}
    return self.flatMap(callback)
    // expected-complete-and-tns-warning @-1 {{passing non-Sendable parameter 'callback' to function expecting a '@Sendable' closure}}
  }

  @inlinable
  @available(*, deprecated, message: "Please don't pass file:line:, there's no point.")
  public func flatMapErrorThrowing(file: StaticString = #file, line: UInt = #line, _ callback: @escaping (Error) throws -> T) -> Future<T> {
    return self.flatMapErrorThrowing(callback)
    // expected-complete-and-tns-warning @-1 {{function call causes an infinite recursion}}
  }
}
