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

protocol Exponential {
// expected-error@-1 {{cannot build rewrite system for protocol; concrete type size limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A where A == (A, A)
}

class Base<T> {}

class Derived<T, U> : Base<(T, U)> {}

protocol TooManyDifferences {
// expected-error@-1 {{cannot build rewrite system for protocol; concrete type difference limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A1 where A1: Derived<B, C>, A2: Base<B>, A1 == A2
  associatedtype A2
  associatedtype B
  associatedtype C
}

protocol M0 {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A: M0
  associatedtype B: M0
  associatedtype C: M0
    where A.B == Self, C.A == B.C  // expected-error *{{is not a member type}}
}

protocol M1 {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A: M1
  associatedtype B: M1
  associatedtype C: M1
    where C.A.C == A, A.B.A == A  // expected-error *{{is not a member type}}
}

protocol M2 {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A: M2
  associatedtype B: M2
  associatedtype C: M2
    where B.A == A.B, C.A == A, A.C == A  // expected-error *{{is not a member type}}
}

protocol M3 {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A: M3
  associatedtype B: M3
    where A.A.A == A, A.B.B.A == B.B  // expected-error *{{is not a member type}}
}

protocol M4 {
// expected-error@-1 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@-2 {{failed rewrite rule is }}
  associatedtype A: M4
  associatedtype B: M4
    where B.A.A == A.A.B, A.B.A == A.A  // expected-error *{{is not a member type}}
}
