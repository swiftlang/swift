// RUN: %target-typecheck-verify-swift

protocol P1 where A == Never {
  associatedtype A
}
struct S1: P1 {}

protocol P2a {
  associatedtype A
}
protocol P2b: P2a where A == Never {}
protocol P2c: P2b {}
struct S2a: P2b {}
struct S2b: P2c {}

// Fixed type witnesses can reference dependent members.
protocol P3a {
  associatedtype A
  associatedtype B
}
protocol P3b: P3a where A == [B] {}
struct S3: P3b {
  typealias B = Never
}

protocol P4a where A == [B] {
  associatedtype A
  associatedtype B
}
protocol P4b {}
extension P4b {
  typealias B = Self
}
struct S4: P4a, P4b {}

// Self is a valid fixed type witness.
protocol P5a {
  associatedtype A
}
protocol P5b: P5a where A == Self {}
struct S5<X>: P5b {} // OK, A := S5<X>


protocol P6 where A == Never { // expected-error {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A : P6'}}
  // expected-note@+1 {{protocol requires nested type 'A}}
  associatedtype A: P6
}
struct S6: P6 {} // expected-error {{type 'S6' does not conform to protocol 'P6'}}

protocol P7a where A == Never {
  associatedtype A
}
// expected-error@+1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Bool'}}
protocol P7b: P7a where A == Bool {}
struct S7: P7b {}

protocol P8a where A == Never {
  associatedtype A
}
protocol P8b where A == Bool {
  associatedtype A
}
do {
  struct Conformer: P8a, P8b {}
  // expected-error@-1 {{'P8b' requires the types 'Conformer.A' (aka 'Never') and 'Bool' be equivalent}}
  // expected-note@-2 {{requirement specified as 'Self.A' == 'Bool' [with Self = Conformer]}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P8b'}}
}

protocol P9a where A == Never {
  associatedtype A
}
protocol P9b: P9a {
  associatedtype A
}
struct S9a: P9b {}
// expected-error@+3 {{type 'S9b' does not conform to protocol 'P9a'}}
// expected-error@+2 {{'P9a' requires the types 'S9b.A' (aka 'Bool') and 'Never' be equivalent}}
// expected-note@+1 {{requirement specified as 'Self.A' == 'Never' [with Self = S9b]}}
struct S9b: P9b {
  typealias A = Bool
}
struct S9c: P9b { // OK, S9c.A does not contradict Self.A == Never.
  typealias Sugar = Never
  typealias A = Sugar
}

protocol P10a where A == Never {
  associatedtype A
}
protocol P10b {}
extension P10b {
  typealias A = Bool
}
// FIXME: 'P10 extension.A' should not be considered a viable type witness;
//  instead, the compiler should infer A := Never and synthesize S10.A.
// expected-error@+3 {{type 'S10' does not conform to protocol 'P10a'}}
// expected-error@+2 {{'P10a' requires the types 'S10.A' (aka 'Bool') and 'Never' be equivalent}}
// expected-note@+1 {{requirement specified as 'Self.A' == 'Never' [with Self = S10]}}
struct S10: P10b, P10a {}

protocol P11a {
  associatedtype A
}
protocol P11b: P11a where A == Never {}
protocol Q11 {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
}
do {
  struct Conformer: Q11, P11b {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P11b'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P11a'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'Q11'}}
}

protocol P12 where A == B {
  associatedtype A
  associatedtype B
  func foo(arg: A)
}
struct S12: P12 {
  func foo(arg: Never) {}
}

protocol P13a {
  associatedtype A
  func foo(arg: A)
}
protocol P13b {
  associatedtype B
}
protocol P13c: P13a, P13b where A == B {}
struct S13: P13c {
  func foo(arg: Never) {}
}

protocol P14 {
  associatedtype A = Array<Self>
}
do {
  struct Outer<Element> {
    struct Conformer: P14 {}
  }
}

protocol P15a {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B = Never // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol P15b: P15a where A == B {}
do {
  struct Conformer: P15b {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P15a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P15b'}}
}

protocol P16a where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B = Never // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol P16b: P16a {}
do {
  struct Conformer: P16b {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P16a'}}
}

protocol P17a where A == Never {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol P17b {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol P17c where A == Never {
  associatedtype A
  associatedtype B = A
}
protocol P17d {
  associatedtype A = B
  associatedtype B = Int
}
do {
  struct Conformer1: P17a {} // expected-error {{type 'Conformer1' does not conform to protocol 'P17a'}}
  struct Conformer2<A>: P17b {} // expected-error {{type 'Conformer2<A>' does not conform to protocol 'P17b'}}
  struct Conformer3: P17c {}
  struct Conformer4<A>: P17d {}
}

protocol P18 {
  associatedtype A = B
  associatedtype B = C
  associatedtype C = (D) -> D
  associatedtype D
}
do {
  struct Conformer<D>: P18 {}
}

protocol P19 where Self == A {
  associatedtype A
  associatedtype B = (A, A)
}
do {
  struct Conformer: P19 {}
}

protocol P20 where A == B.Element, B == B.SubSequence, C.Element == B.Element {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B: Collection
  associatedtype C: Collection = Array<Character> // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}
}
do {
  struct Conformer: P20 { // expected-error {{type 'Conformer' does not conform to protocol 'P20'}}
    typealias B = Substring
  }
}

protocol P21 where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B = C // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
  associatedtype C // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}
}
do {
  struct Conformer<C>: P21 {} // expected-error {{type 'Conformer<C>' does not conform to protocol 'P21'}}
}

protocol P22 where A == B, C == D {
  associatedtype A
  associatedtype B
  associatedtype C = B
  associatedtype D
}
do {
  struct Conformer<A>: P22 {}
}

protocol P23 {
  associatedtype A: P23 = B.A // expected-note 2 {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B: P23 = A.B // expected-note 2 {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
do {
  struct Conformer: P23 {} // expected-error {{type 'Conformer' does not conform to protocol 'P23'}}
  struct ConformerGeneric<T>: P23 {} // expected-error {{type 'ConformerGeneric<T>' does not conform to protocol 'P23'}}
}

protocol P24 where A == B.A {
  associatedtype A: P24 // expected-note 2 {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B: P24 = A.B // expected-note 2 {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
do {
  struct Conformer: P24 {} // expected-error {{type 'Conformer' does not conform to protocol 'P24'}}
  struct ConformerGeneric<T>: P24 {} // expected-error {{type 'ConformerGeneric<T>' does not conform to protocol 'P24'}}
}

protocol P25a_1 where A == Int, B == C.Element {
  associatedtype A
  associatedtype B
  associatedtype C: Sequence
}
protocol P25a_2 where A == C.Element, B == Int {
  associatedtype A
  associatedtype B
  associatedtype C: Sequence
}
protocol P25b where A == B {
  associatedtype A
  associatedtype B
}
protocol P25c_1: P25a_1, P25b {}
protocol P25c_2: P25a_2, P25b {}
do {
  struct Conformer1<C: Sequence>: P25c_1 where C.Element == Int {}
  struct Conformer2<C: Sequence>: P25c_2 where C.Element == Int {}
}

protocol P26 where C == B, F == G {
  associatedtype A = Int // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B = A // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
  associatedtype C // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}

  associatedtype D // expected-note {{protocol requires nested type 'D'; add nested type 'D' for conformance}}
  associatedtype E = D // expected-note {{protocol requires nested type 'E'; add nested type 'E' for conformance}}

  associatedtype F // expected-note {{protocol requires nested type 'F'; add nested type 'F' for conformance}}
  associatedtype G = [B] // expected-note {{protocol requires nested type 'G'; add nested type 'G' for conformance}}
}
do {
  struct Conformer<D>: P26 {} // expected-error {{type 'Conformer<D>' does not conform to protocol 'P26'}}
}

protocol P27a where A == Int {
  associatedtype A
}
protocol P27b where A == B.Element {
  associatedtype A
  associatedtype B: Sequence
}
protocol P27c_1: P27a, P27b {}
protocol P27c_2: P27b, P27a {}
do {
  struct Conformer1<B: Sequence>: P27c_1 where B.Element == Int {}
  struct Conformer2<B: Sequence>: P27c_2 where B.Element == Int {}
}

protocol P28a where A == Int {
  associatedtype A
}
protocol P28b where A == Bool {
  associatedtype A
}
protocol P28c where A == Never {
  associatedtype A
}
protocol Q28a: P28a, P28b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Bool' and 'Self.A == Int'}}
protocol Q28b: P28a, P28b, P28c {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Bool'}}
// expected-error@-2 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
// expected-error@-3 {{no type for 'Self.A' can satisfy both 'Self.A == Bool' and 'Self.A == Int'}}
do {
  struct Conformer1: Q28a {}
  // expected-error@-1 {{'P28b' requires the types 'Conformer1.A' (aka 'Int') and 'Bool' be equivalent}}
  // expected-note@-2 {{requirement specified as 'Self.A' == 'Bool' [with Self = Conformer1]}}
  // expected-error@-3 {{type 'Conformer1' does not conform to protocol 'P28b'}}

  struct Conformer2: Q28b {}
  // expected-error@-1 {{'P28c' requires the types 'Conformer2.A' (aka 'Int') and 'Never' be equivalent}}
  // expected-error@-2 {{'P28b' requires the types 'Conformer2.A' (aka 'Int') and 'Bool' be equivalent}}
  // expected-note@-3 {{requirement specified as 'Self.A' == 'Never' [with Self = Conformer2]}}
  // expected-note@-4 {{requirement specified as 'Self.A' == 'Bool' [with Self = Conformer2]}}
  // expected-error@-5 {{type 'Conformer2' does not conform to protocol 'P28b'}}
  // expected-error@-6 {{type 'Conformer2' does not conform to protocol 'P28c'}}
}

protocol P29a where A == Int {
  associatedtype A
  associatedtype B
}
protocol P29b where B == Never {
  associatedtype B
}
protocol P29c where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol Q29a: P29a, P29b, P29c {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
protocol Q29b: P29c, P29a, P29b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  struct Conformer1: Q29a {}
  // expected-error@-1 {{'P29b' requires the types 'Conformer1.B' (aka 'Int') and 'Never' be equivalent}}
  // expected-note@-2 {{requirement specified as 'Self.B' == 'Never' [with Self = Conformer1]}}
  // expected-error@-3 {{type 'Conformer1' does not conform to protocol 'P29b'}}


  struct Conformer2: Q29b {}
  // expected-error@-1 {{type 'Conformer2' does not conform to protocol 'P29a'}}
  // expected-error@-2 {{type 'Conformer2' does not conform to protocol 'P29b'}}
  // expected-error@-3 {{type 'Conformer2' does not conform to protocol 'P29c'}}
}

protocol P30a where A == Int {
  associatedtype A
}
protocol P30b where A == Never {
  associatedtype A
}
protocol P30c where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol Q30: P30c, P30a, P30b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  struct Conformer: Q30 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P30a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P30b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P30c'}}
}

protocol P31a where B == Int {
  associatedtype B
}
protocol P31b where B == Never {
  associatedtype B
}
protocol P31c where B == A {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol Q31: P31c, P31a, P31b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  struct Conformer: Q31 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P31a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P31b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P31c'}}
}

protocol P32a where A == Int {
  associatedtype A
}
protocol P32b where A == Bool {
  associatedtype A
}
protocol P32c where B == Void {
  associatedtype B
}
protocol P32d where B == Never {
  associatedtype B
}
protocol P32e where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
protocol Q32: P32e, P32a, P32b, P32c, P32d {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == ()'}}
// expected-error@-2 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Bool'}}
// expected-error@-3 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
// expected-error@-4 {{no type for 'Self.A' can satisfy both 'Self.A == Bool' and 'Self.A == Int'}}
// expected-error@-5 {{no type for 'Self.A' can satisfy both 'Self.A == ()' and 'Self.A == Int'}}
// expected-error@-6 {{no type for 'Self.A' can satisfy both 'Self.A == ()' and 'Self.A == Bool'}}
do {
  struct Conformer: Q32 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P32a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P32b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P32c'}}
  // expected-error@-4 {{type 'Conformer' does not conform to protocol 'P32d'}}
  // expected-error@-5 {{type 'Conformer' does not conform to protocol 'P32e'}}
}

protocol P33a where A == Int {
  associatedtype A
}
protocol P33b where A == Int {
  associatedtype A
}
protocol Q33: P33a, P33b {}
do {
  struct Conformer: Q33 {}
}

protocol P34a {
  associatedtype A = Void
}
protocol P34b {
  associatedtype A = Never
}
protocol Q34a: P34a, P34b {}
protocol Q34b: P34b, P34a {}
protocol Q34c: P34a, P34b {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
}
do {
  struct Conformer1: Q34a {}
  struct Conformer2: Q34b {}
  struct Conformer3: Q34c {} // expected-error {{type 'Conformer3' does not conform to protocol 'Q34c'}}
}

protocol P35 {
  associatedtype A
  associatedtype B = Array<C>
  associatedtype C = Array<D>
  associatedtype D = Array<A>
}
do {
  struct Conformer: P35 {
    typealias A = Never
  }
  struct ConformerGeneric<A>: P35 {}
}

struct G36<S: P36> {}
protocol P36 {
  // FIXME: Don't create and expose malformed types like 'G36<Never>' -- check
  // non-dependent type witnesses first.
  // expected-note@+1 {{default type 'G36<Never>' for associated type 'A' (from protocol 'P36') does not conform to 'P36'}}
  associatedtype A: P36 = G36<B>
  associatedtype B: P36 = Never
}
do {
  struct Conformer: P36 {} // expected-error {{type 'Conformer' does not conform to protocol 'P36'}}
}

protocol P37a {
  associatedtype A
}
protocol P37b {
  associatedtype B : P37a
  associatedtype C where C == B.A // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}
}
do {
  struct Conformer1<C>: P37b {
    struct Inner: P37a { typealias A = C }

    typealias B = Inner
  }

  struct Conformer2<T>: P37b { // expected-error {{type 'Conformer2<T>' does not conform to protocol 'P37b'}}
    struct Inner: P37a { typealias A = T }

    typealias B = Inner
  }
}

protocol P38a {
  associatedtype A
}
protocol P38b {
  associatedtype B
}
protocol P38c: P38a, P38b where A == B {}
do {
  struct Conformer<T>: P38c {
    typealias A = Self
  }
}

protocol P39 {
  associatedtype A: P39
  associatedtype B = A.C
  associatedtype C = Never
}
do {
  struct Conformer: P39 {
    typealias A = Self
  }
}

protocol P40a {
  associatedtype A
  associatedtype B = (A, A)
}
protocol P40b: P40a {
  override associatedtype A = Int
}
protocol P40c: P40b {
  override associatedtype A
  override associatedtype B
}
do {
  struct Conformer: P40c {}
}

protocol P41 {
  associatedtype A where A == B.A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B: P41 = Self // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
}
do {
  struct Conformer: P41 {} // expected-error{{type 'Conformer' does not conform to protocol 'P41'}}
}

protocol P42a {
  associatedtype B: P42b
}
protocol P42b: P42a {
  associatedtype A = B.A
}
do {
  struct Conformer<B: P42b>: P42b {}
}

protocol P43a {
  associatedtype A: P43a
  associatedtype B
}
protocol P43b: P43a {
  associatedtype C where C == A.B // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}
}
do {
  struct Conformer<B: P43a>: P43b { // expected-error {{type 'Conformer<B>' does not conform to protocol 'P43b'}}
    typealias A = Conformer<B.A>
  }
}

protocol P44 {
  associatedtype A: P44
  associatedtype B // expected-note 2{{protocol requires nested type 'B'; add nested type 'B' for conformance}}
  associatedtype C where C == A.B // expected-note 3{{protocol requires nested type 'C'; add nested type 'C' for conformance}}
}
do {
  struct Conformer1<T: P44>: P44 { // expected-error {{type 'Conformer1<T>' does not conform to protocol 'P44'}}
    typealias B = T.A
    typealias A = Conformer1<T.A>
  }

  struct Conformer2<B: P44>: P44 { // expected-error {{type 'Conformer2<B>' does not conform to protocol 'P44'}}
    typealias A = Conformer2<B.A>
  }

  struct Conformer3<B>: P44 { // expected-error {{type 'Conformer3<B>' does not conform to protocol 'P44'}}
    typealias A = Conformer3<Int>
  }
}

protocol P45 {
  associatedtype A // expected-note {{protocol requires nested type 'A'; add nested type 'A' for conformance}}
  associatedtype B: P45 = Conformer45<D> // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
  associatedtype C where C == B.A // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}
  associatedtype D = Never // expected-note {{protocol requires nested type 'D'; add nested type 'D' for conformance}}
}
struct Conformer45<A>: P45 {} // expected-error {{type 'Conformer45<A>' does not conform to protocol 'P45'}}

protocol P46 {
  associatedtype A: P46
  associatedtype B // expected-note {{protocol requires nested type 'B'; add nested type 'B' for conformance}}
  associatedtype C where C == A.B // expected-note {{protocol requires nested type 'C'; add nested type 'C' for conformance}}

  func method(_: B)
}
do {
  struct Conformer<T: P46>: P46 { // expected-error {{type 'Conformer<T>' does not conform to protocol 'P46'}}
    typealias A = Conformer<T.A>

    func method(_: T) {}
  }
}


protocol P47 {
  associatedtype A
}
do {
  struct Outer<A> {
    struct Inner: P47 {}
  }
}

protocol P48a { associatedtype A = Int }
protocol P48b { associatedtype B }
protocol P48c: P48a, P48b where A == B {}
do {
  struct Conformer: P48c {}
}
