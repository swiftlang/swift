// RUN: %target-typecheck-verify-swift

struct RequiresStrideable<T: Strideable> { }

extension CountableRange {
  func testStrideable() {
    _ = RequiresStrideable<Bound>()
  }

  func foo() { }
}

extension Range {
  func foo() { } // not a redefinition
}

struct RequiresHashable<T: Hashable> { }

extension DictionaryIndex {
  func testHashable() {
    _ = RequiresHashable<Key>()
  }
}

extension CountableRange where Element == Int {
  func getLowerBoundAsInt() -> Int { return lowerBound }
}
