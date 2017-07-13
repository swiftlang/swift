// RUN: %target-typecheck-verify-swift -enable-recursive-constraints

protocol P {
  associatedtype Assoc : P

  var assoc: Assoc { get }
}

func testP<T: P>(_ t: T) {
  testP(t.assoc)
  testP(t.assoc.assoc)
  testP(t.assoc.assoc.assoc) // FIXME: expected-error{{argument type 'T.Assoc.Assoc.Assoc' does not conform to expected type 'P'}}
}
