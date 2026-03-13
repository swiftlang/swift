// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/79367

// 'Failure' type witness inference should still take place when
// the 'next()' witness is in a different extension than the
// conformance.

struct AsyncIteratorImpl<Element>: AsyncIteratorProtocol {}

extension AsyncIteratorImpl {
  func next() async throws -> Element? {
    fatalError()
  }
}
