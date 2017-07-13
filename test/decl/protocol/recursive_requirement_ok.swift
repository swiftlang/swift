// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype Assoc : P

  var assoc: Assoc { get }
}

func testP<T: P>(_ t: T) {
  testP(t.assoc)
  testP(t.assoc.assoc)
  testP(t.assoc.assoc.assoc)
  testP(t.assoc.assoc.assoc.assoc.assoc.assoc.assoc)
}

// SR-5485
protocol P1 {
  associatedtype X : P2
}
protocol P2 {
  associatedtype Y : P1 where Y.X == Self // expected-note{{conformance constraint 'Self.Z': 'P1' implied here}}
  associatedtype Z : P1 // expected-warning{{redundant conformance constraint 'Self.Z': 'P1'}}
}
