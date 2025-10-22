// RUN: not %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/47152

struct V<T> : BidirectionalCollection {}
struct S {
  func bar<T>(_ to: T.Type) -> V<T> {
    return V<T>()
  }
}

extension S {
  func foo<R>(_ body: (UnsafeBufferPointer<UTF16.CodeUnit>) -> R) -> R {
    return Array(self.bar(UTF16.self)).withUnsafeBufferPointer(body)
  }
}
