// RUN: %target-typecheck-verify-swift

protocol ABA // expected-error {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-1 {{failed rewrite rule is [ABA:A].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:A] => [ABA:A].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B].[ABA:B]}}
    where A.B == A.B.A { // expected-error *{{is not a member type}}
  associatedtype A : ABA
  associatedtype B : ABA
}

protocol Undecidable // expected-error {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-1 {{failed rewrite rule is [Undecidable:A].[Undecidable:C].[Undecidable:C].[Undecidable:C].[Undecidable:D].[Undecidable:C].[Undecidable:E].[Undecidable:E].[Undecidable:B].[Undecidable:B].[Undecidable:A].[Undecidable:B].[Undecidable:C].[Undecidable:C].[Undecidable:C].[Undecidable:D].[Undecidable:D] => [Undecidable:A].[Undecidable:C].[Undecidable:C].[Undecidable:C].[Undecidable:D].[Undecidable:C].[Undecidable:C].[Undecidable:C].[Undecidable:C].[Undecidable:D].[Undecidable:D].[Undecidable:E].[Undecidable:A].[Undecidable:B]}}
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
// expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P2] => τ_0_0.[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T]}}

extension P1 where Self : P2 {}
// expected-error@-1 {{cannot build rewrite system for generic signature; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is τ_0_0.[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P2] => τ_0_0.[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T]}}

struct S<U : P1> : P1 {
  typealias T = S<S<U>>
}

protocol P3Base {
  associatedtype T : P1
  associatedtype U : P1
}

protocol P3 : P3Base where T == S<U> {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is [P3:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[concrete: S<S<S<S<S<S<S<S<S<S<S<S<S<S<[P3:U]>>>>>>>>>>>>>>] => [P3:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T].[P1:T]}}
}
