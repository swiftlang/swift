// RUN: %target-swift-frontend -emit-sil -debug-diagnostic-names -verify %s

public protocol Fooable {
  func foo() -> Int
}

extension Int : Fooable {
  public func foo() -> Int {
    self
  }
}

extension Array: Comparable where Element: Comparable { // expected-warning{{extension_retroactive_conformance}}
                                                        // expected-note@-1{{extension_retroactive_conformance_silence}}
	public static func < (lhs: Self, rhs: Self) -> Bool {
    true
	}
}

extension Array : Fooable where Element : Fooable {
  public func foo() -> Int {
    0
  }
}

struct Wrapper<Symbol: Hashable & Comparable & Fooable>: Hashable {
	var partitions: PartitionSet<Array<Symbol>>
}

struct PartitionSet<Symbol: Hashable & Comparable & Fooable>: Equatable, Hashable {
	var partitions: Set<Set<Symbol>>
}


