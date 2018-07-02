// RUN: %target-typecheck-verify-swift

protocol P1 {}
protocol P2 {}

struct S01<T : Any> {} // expected-warning {{redundant conformance constraint 'T': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

struct S02<T> where T : Any {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{15-29=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

struct S04<T : P1> where T : Any {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{20-34=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

struct S05<T : Any> where T : P1 {} // expected-warning {{redundant conformance constraint 'T': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

struct S06<T : Any> where T : Any {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}
// expected-warning@-3 {{redundant conformance constraint 'T': 'Any'}} {{21-35=}}
// expected-note@-4 {{all types implicitly conform to 'Any'}}

struct S07<T> where T : Any, T : P1 {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}} {{21-30=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

struct S08<T> where T : P1, T : Any {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}} // FIX-ME(SR-8102): Add expected fix-it {{27-36=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

struct S09<T> where T : P1, T : Any, T : P2 {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}} {{29-38=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

struct S10<T> where T : P1, T : P2, T : Any {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}} // FIX-ME(SR-8102): Add expected fix-it {{35-44=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}

struct S11<T, U> {}
extension S11 where T : Any {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{15-29=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

extension S11 where T : Any, T : P1 {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{21-30=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

extension S11 where T : Any, U : Any {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'Any'}} {{21-30=}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}
// expected-warning@-3 {{redundant conformance constraint 'U': 'Any'}} // FIX-ME(SR-8102): Add expected fix-it {{28-37=}}
// expected-note@-4 {{all types implicitly conform to 'Any'}}


protocol P3 {
  // expected-warning@-1 {{redundant conformance constraint 'Self.X1': 'Any'}}
  // expected-note@-2 {{all types implicitly conform to 'Any'}}
  // expected-warning@-3 {{redundant conformance constraint 'Self.X2': 'Any'}} {{21-35=}}
  // expected-note@-4 {{all types implicitly conform to 'Any'}}

  associatedtype X1 : Any
  associatedtype X2 where X2 : Any
}

protocol P4 {
  // expected-warning@-1 {{redundant conformance constraint 'Self.X1': 'Any'}}
  // expected-note@-2 {{all types implicitly conform to 'Any'}}
  // expected-warning@-3 {{redundant conformance constraint 'Self.X1': 'Any'}}
  // expected-note@-4 {{all types implicitly conform to 'Any'}}
  // expected-warning@-5 {{redundant conformance constraint 'Self.X2': 'Any'}} {{27-37=}}
  // expected-note@-6 {{all types implicitly conform to 'Any'}}
  // expected-warning@-7 {{redundant conformance constraint 'Self.X2': 'Any'}} // FIX-ME(SR-8102): Add expected fix-it {{35-45=}}
  // expected-note@-8 {{all types implicitly conform to 'Any'}}
  // expected-warning@-9 {{redundant conformance constraint 'Self.X3': 'Any'}} {{27-37=}}
  // expected-note@-10 {{all types implicitly conform to 'Any'}}
  // expected-warning@-11 {{redundant conformance constraint 'Self.X3': 'Any'}} {{37-47=}}
  // expected-note@-12 {{all types implicitly conform to 'Any'}}

  associatedtype X1 : Any, Any // expected-error {{duplicate inheritance from 'Any'}} {{26-31=}}
  associatedtype X2 where X2 : Any, X2 : Any
  associatedtype X3 where X3 : Any, X3 : Any, X3 : P1
}

protocol P5 : Any {} // expected-warning {{redundant conformance constraint 'Self': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

protocol P6 : Any & Any {} // expected-warning {{redundant conformance constraint 'Self': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

protocol P7 : Any, Any {}
// expected-warning@-1 {{redundant conformance constraint 'Self': 'Any'}}
// expected-note@-2 {{all types implicitly conform to 'Any'}}
// expected-warning@-3 {{redundant conformance constraint 'Self': 'Any'}}
// expected-note@-4 {{all types implicitly conform to 'Any'}}
// expected-error@-5 {{duplicate inheritance from 'Any'}} {{18-23=}}

func f1<T : Any>(_ x: T) {} // expected-warning {{redundant conformance constraint 'T': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

func f2<T>(_ x: T) where T : Any {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{20-34=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

typealias AnyAlias = Any
func f3<T>(_ x: T) where T : AnyAlias {} // expected-warning {{redundant conformance constraint 'T': 'AnyAlias' (aka 'Any')}} {{20-39=}}
// expected-note@-1 {{all types implicitly conform to 'AnyAlias' (aka 'Any')}}

func f4<T : Any & Any>(_ x: T) {} // expected-warning {{redundant conformance constraint 'T': 'Any'}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

func f5<T>(_ x: T) where T : Any & Any {} // expected-warning {{redundant conformance constraint 'T': 'Any'}} {{20-40=}}
// expected-note@-1 {{all types implicitly conform to 'Any'}}

typealias AnyAnyAlias = Any & Any
func f6<T : AnyAnyAlias>(_ x: T) where T : Any {}
// expected-warning@-1 {{redundant conformance constraint 'T': 'AnyAnyAlias' (aka 'Any')}}
// expected-note@-2 {{all types implicitly conform to 'AnyAnyAlias' (aka 'Any')}}
// expected-warning@-3 {{redundant conformance constraint 'T': 'Any'}} {{34-48=}}
// expected-note@-4 {{all types implicitly conform to 'Any'}}

// Don't specifically warn on 'Any & P' constraints.
// FIX-ME(SR-8082): Implement a general diagnostic for the usage of Any & P1.
func f7<T : Any & P1>(_ x: T) {}
func f8<T>(_ x: T) where T : P1 & Any {}

