// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// https://github.com/apple/swift/issues/57264

protocol P {
  associatedtype A
  associatedtype AS: Q where AS.B == A
}

protocol Q {
  associatedtype B
}

struct S1<T : P> where T.AS.B == T.A {}
// expected-warning@-1 {{redundant same-type constraint 'T.AS.B' == 'T.A'}}

struct S2<T: P> {
  struct Nested where T.AS.B == T.A {}
  // expected-warning@-1 {{redundant same-type constraint 'T.AS.B' == 'T.A'}}
}

extension S2 where T.AS.B == T.A {}
// expected-warning@-1 {{redundant same-type constraint 'T.AS.B' == 'T.A'}}

extension P where AS.B == A {}
// expected-warning@-1 {{redundant same-type constraint 'Self.AS.B' == 'Self.A'}}

extension P where Self : P {}
// expected-warning@-1 {{redundant conformance constraint 'Self' : 'P'}}
