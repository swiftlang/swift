// RUN: %target-swift-frontend -typecheck -verify %s
// REQUIRES: concurrency

// https://github.com/apple/swift/issues/59909
struct Future<T> { }

extension Future {
  @preconcurrency
  func flatMap<NewValue>(_ callback: @escaping @Sendable (T) -> Future<NewValue>) -> Future<NewValue> { // #1
    fatalError()
  }
}

extension Future {
  @available(*, deprecated, message: "")
  func flatMap<NewValue>(file: StaticString = #file, line: UInt = #line, _ callback: @escaping (T) -> Future<NewValue>) -> Future<NewValue> { // #2
    return self.flatMap(callback)
  }
}
