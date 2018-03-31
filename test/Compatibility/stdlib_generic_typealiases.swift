// RUN: %target-typecheck-verify-swift

struct RequiresStrideable<T: Strideable> { }

extension CountableRange { // expected-warning{{'CountableRange' is deprecated: renamed to 'Range'}}
  // expected-note@-1{{use 'Range' instead}}{{11-25=Range}}
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
  // expected-warning@-1{{'CountableRange' is deprecated: renamed to 'Range'}}
  // expected-note@-2{{use 'Range' instead}}
  func getLowerBoundAsInt() -> Int { return lowerBound }
}
