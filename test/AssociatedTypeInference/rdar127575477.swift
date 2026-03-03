// RUN: not %target-typecheck-verify-swift -verify-ignore-unrelated

// This is highly invalid, so just don't crash.

struct G<T> {}

extension G: Collection {
  typealias SubSequence = DoesNotExist
  typealias Index = DoesNotExist

  subscript(position: Index) -> Element { fatalError() }
}

extension G: BidirectionalCollection where T: BidirectionalCollection {}
