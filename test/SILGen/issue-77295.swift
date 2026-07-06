// RUN: %target-swift-emit-silgen %s -verify

// https://github.com/swiftlang/swift/issues/77295 - Make sure this compiles.
extension Optional {
  func foo<E: Error>(orThrow error: @autoclosure () -> E) throws(E) -> Wrapped {
    switch self {
    case .none:
      throw error()
    case .some(let wrapped):
      wrapped
    }
  }
}
