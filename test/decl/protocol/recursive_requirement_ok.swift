// RUN: %target-typecheck-verify-swift -enable-recursive-constraints

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
