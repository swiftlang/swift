// RUN: %target-swift-frontend -typecheck -verify %s
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
    return self.flatMap(callback)
  }

  @inlinable
  @available(*, deprecated, message: "Please don't pass file:line:, there's no point.")
  public func flatMapErrorThrowing(file: StaticString = #file, line: UInt = #line, _ callback: @escaping (Error) throws -> T) -> Future<T> {
    return self.flatMapErrorThrowing(callback)
  }
}
