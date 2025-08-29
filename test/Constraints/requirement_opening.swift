// RUN: %target-typecheck-verify-swift

struct K<U> {} // expected-note 6{{'U' declared as parameter to type 'K'}}
protocol Q {}

struct S1<T: Q>: ExpressibleByArrayLiteral { // expected-note 2{{where 'T' = 'K<Int>'}}
  init(_ x: T) {}
  init(arrayLiteral: T...) {}
}

typealias R1<T: Q> = S1<T> // expected-note {{where 'T' = 'K<Int>'}} expected-note 2{{where 'T' = 'K<U>'}}

func foo(_ x: K<Int>) {
  let _ = [x] as S1<K> // expected-error {{generic struct 'S1' requires that 'K<Int>' conform to 'Q'}}
  let _ = [x] as R1<K> // expected-error {{generic type alias 'R1' requires that 'K<Int>' conform to 'Q'}}

  let _: S1<K> = [x]   // expected-error {{generic struct 'S1' requires that 'K<Int>' conform to 'Q'}}
  // FIXME: We ought to be able to infer 'U' here.
  let _: R1<K> = [x] // expected-error 2 {{generic type alias 'R1' requires that 'K<U>' conform to 'Q'}}
  // expected-error@-1 2 {{generic parameter 'U' could not be inferred}}
}

protocol P2 {
  associatedtype A
}

struct S2<A: Q>: P2 {} // expected-note 3{{where 'A' = 'K<Int>'}}
typealias R2<A: Q> = S2<A> // expected-note 2{{where 'A' = 'K<Int>'}} expected-note 2{{where 'A' = 'K<U>'}}

// Same as S2, but without the Q requirement.
struct S3<A>: P2 {}
typealias R3<A: Q> = S3<A> // expected-note {{where 'A' = 'K<Int>'}} expected-note {{where 'A' = 'K<U>'}}

func foo<T: P2>(_ y: T.A.Type) -> T {}
let _ = foo(K<Int>.self) as S2<K> // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
let _ = foo(K<Int>.self) as R2<K> // expected-error {{generic type alias 'R2' requires that 'K<Int>' conform to 'Q'}}
let _ = foo(K<Int>.self) as R3<K> // expected-error {{generic type alias 'R3' requires that 'K<Int>' conform to 'Q'}}

let _: S2<K> = foo(K<Int>.self)   // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
// FIXME: We ought to be able to infer 'U' here.
let _: R2<K> = foo(K<Int>.self)   // expected-error 2{{generic type alias 'R2' requires that 'K<U>' conform to 'Q'}}
// expected-error@-1 2 {{generic parameter 'U' could not be inferred}}
let _: R3<K> = foo(K<Int>.self)   // expected-error {{generic type alias 'R3' requires that 'K<U>' conform to 'Q'}}
// expected-error@-1 2 {{generic parameter 'U' could not be inferred}}

func foo<T: P2>(_ x: T.Type, _ y: T.A.Type) {}
foo(S2<_>.self, K<Int>.self) // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
foo(R2<_>.self, K<Int>.self) // expected-error {{generic type alias 'R2' requires that 'K<Int>' conform to 'Q'}}

struct S4<T: Q> { // expected-note {{where 'T' = 'Int'}}
  init(_ x: T) {}
}

_ = S4<_>(0) // expected-error {{generic struct 'S4' requires that 'Int' conform to 'Q'}}
