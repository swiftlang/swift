// RUN: %target-typecheck-verify-swift

// rdar://problem/31401161
class C1 {}

protocol P1 {
  associatedtype Element
}

protocol P2 : P1 {
  associatedtype SubSequence : P1 // expected-note{{'SubSequence' declared here}}
}

protocol P3 : P2 {
  associatedtype SubSequence : P2 // expected-warning{{redeclaration of associated type 'SubSequence' from protocol 'P2' is better expressed as a 'where' clause on the protocol}}
}

func foo<S>(_: S) where S.SubSequence.Element == C1, S : P3 {}

// Invalid where clauses
protocol InvalidWhereClause1 where Self: AnyObject {}
// expected-error@-1 {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}

class AlsoBad {}

protocol InvalidWhereClause2 {
  associatedtype T where Self: AlsoBad
  // expected-error@-1 {{constraint with subject type of 'Self' is not supported; consider adding requirement to protocol inheritance clause instead}}
}
