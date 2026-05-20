// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple -swift-version 6 -verify -enable-sil-opaque-values %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

// Test that init_existential_value with an opened archetype concrete type does
// not produce a false positive in the region-based isolation analysis under the
// opaque-values SIL lowering. The fake protocols below mimic the
// Sequence/IteratorProtocol shape without depending on the standard library,
// which does not compile with -enable-sil-opaque-values.

////////////////////////
// MARK: Declarations //
////////////////////////

public protocol FakeIteratorProtocol {
  mutating func nextElement()
}

public protocol FakeSequence {
  associatedtype Iterator: FakeIteratorProtocol
  func makeIterator() -> Iterator
}

/////////////////
// MARK: Tests //
/////////////////

// Iterating over an existential sequence parameter in a nonisolated actor init
// must not produce a false positive RBI error in opaque-values mode.
public actor ActorWithFakeSequenceInit {
  public init(sequence: any FakeSequence) {
    var iter: any FakeIteratorProtocol = sequence.makeIterator()
    iter.nextElement()
  }
}

@MainActor
public class MainActorClassWithFakeSequenceInit {
  public init(sequence: any FakeSequence) {
    var iter: any FakeIteratorProtocol = sequence.makeIterator()
    iter.nextElement()
  }
}

public func nonisolatedFuncWithFakeSequence(sequence: any FakeSequence) {
  var iter: any FakeIteratorProtocol = sequence.makeIterator()
  iter.nextElement()
}
