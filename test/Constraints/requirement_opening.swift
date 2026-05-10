// RUN: %target-typecheck-verify-swift

struct K<U> {}
protocol Q {}
struct ConformingType: Q {}

struct S1<T: Q>: ExpressibleByArrayLiteral { // expected-note 2{{where 'T' = 'K<Int>'}}
  init(_ x: T) {}
  init(arrayLiteral: T...) {}
}

typealias R1<T: Q> = S1<T> // expected-note 2{{where 'T' = 'K<Int>'}}

func foo(_ x: K<Int>) {
  let _ = [x] as S1<K> // expected-error {{generic struct 'S1' requires that 'K<Int>' conform to 'Q'}}
  let _ = [x] as R1<K> // expected-error {{generic type alias 'R1' requires that 'K<Int>' conform to 'Q'}}

  let _: S1<K> = [x] // expected-error {{generic struct 'S1' requires that 'K<Int>' conform to 'Q'}}
  let _: R1<K> = [x] // expected-error {{generic type alias 'R1' requires that 'K<Int>' conform to 'Q'}}
}

protocol P2 {
  associatedtype A
}

struct S2<A: Q>: P2 {} // expected-note 3{{where 'A' = 'K<Int>'}}
typealias R2<A: Q> = S2<A> // expected-note 3{{where 'A' = 'K<Int>'}}

// Same as S2, but without the Q requirement.
struct S3<A>: P2 {}
typealias R3<A: Q> = S3<A> // expected-note 2{{where 'A' = 'K<Int>'}}

func foo<T: P2>(_ y: T.A.Type) -> T {}
let _ = foo(K<Int>.self) as S2<K> // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
let _ = foo(K<Int>.self) as R2<K> // expected-error {{generic type alias 'R2' requires that 'K<Int>' conform to 'Q'}}
let _ = foo(K<Int>.self) as R3<K> // expected-error {{generic type alias 'R3' requires that 'K<Int>' conform to 'Q'}}

let _: S2<K> = foo(K<Int>.self)   // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
let _: R2<K> = foo(K<Int>.self)   // expected-error {{generic type alias 'R2' requires that 'K<Int>' conform to 'Q'}}
let _: R3<K> = foo(K<Int>.self)   // expected-error {{generic type alias 'R3' requires that 'K<Int>' conform to 'Q'}}

func foo<T: P2>(_ x: T.Type, _ y: T.A.Type) {}
foo(S2<_>.self, K<Int>.self) // expected-error {{generic struct 'S2' requires that 'K<Int>' conform to 'Q'}}
foo(R2<_>.self, K<Int>.self) // expected-error {{generic type alias 'R2' requires that 'K<Int>' conform to 'Q'}}

struct S4<T: Q> { // expected-note {{where 'T' = 'Int'}}
  init(_ x: T) {}
}

_ = S4<_>(0) // expected-error {{generic struct 'S4' requires that 'Int' conform to 'Q'}}

func testLocalOuterGeneric<T>(_ x: T) {
  typealias X<U: Q> = (T, U) // expected-note {{where 'U' = 'String'}}
  let _: X<_> = (x, "") // expected-error {{generic type alias 'X' requires that 'String' conform to 'Q'}}
  let _: X<_> = (x, ConformingType())
}

struct TestParentGeneric<T> {
  typealias X<U: Q> = (T, U) // expected-note {{where 'U' = 'String'}}
  func bar(_ x: T) {
    let _: X<_> = (x, "") // expected-error {{generic type alias 'X' requires that 'String' conform to 'Q'}}
    let _: X<_> = (x, ConformingType())
  }
}
