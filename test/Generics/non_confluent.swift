// RUN: %target-typecheck-verify-swift -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on

protocol ABA // expected-error {{cannot build rewrite system for protocol; depth limit exceeded}}
    where A.B == A.B.A { // expected-error *{{is not a member type}}
  associatedtype A : ABA
  associatedtype B : ABA
}

protocol Undecidable // expected-error {{cannot build rewrite system for protocol; depth limit exceeded}}
    where A.C == C.A, // expected-error *{{is not a member type}}
          A.D == D.A, // expected-error *{{is not a member type}}
          B.C == C.B, // expected-error *{{is not a member type}}
          B.D == D.B, // expected-error *{{is not a member type}}
          C.E == E.C.A, // expected-error *{{is not a member type}}
          D.E == E.D.B, // expected-error *{{is not a member type}}
          C.C.A == C.C.A.E { // expected-error *{{is not a member type}}
  associatedtype A : Undecidable
  associatedtype B : Undecidable
  associatedtype C : Undecidable
  associatedtype D : Undecidable
  associatedtype E : Undecidable
}

protocol P1 {
  associatedtype T : P1
}

protocol P2 {
  associatedtype T : P2
}

func foo<T : P1 & P2>(_: T) {}
// expected-error@-1 {{cannot build rewrite system for generic signature; depth limit exceeded}}

extension P1 where Self : P2 {}
// expected-error@-1 {{cannot build rewrite system for generic signature; depth limit exceeded}}