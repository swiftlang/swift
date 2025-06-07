// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -dump-type-witness-systems %s 2>&1 | %FileCheck %s

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
// CHECK-LABEL: Abstract type witness system for conformance of S3 to P3a: {
// CHECK-NEXT: A => [Self.B],
// CHECK-NEXT: }
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
// CHECK-LABEL: Abstract type witness system for conformance of S4 to P4a: {
// CHECK-NEXT: A => [Self.B] (preferred),
// CHECK-NEXT: }
struct S4: P4a, P4b {}

// Self is a valid fixed type witness.
protocol P5a {
  associatedtype A
}
protocol P5b: P5a where A == Self {}
// CHECK-LABEL: Abstract type witness system for conformance of S5<X> to P5a: {
// CHECK-NEXT: A => Self,
// CHECK-NEXT: }
struct S5<X>: P5b {} // OK, A := S5<X>


protocol P6 where A == Never { // expected-error {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A : P6'}}
  // expected-note@+1 {{protocol requires nested type 'A}}
  associatedtype A: P6
}
struct S6: P6 {} // expected-error {{type 'S6' does not conform to protocol 'P6'}} expected-note {{add stubs for conformance}}

protocol P7a where A == Never {
  associatedtype A
}
// expected-error@+1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Bool'}}
protocol P7b: P7a where A == Bool {}
struct S7: P7b {}

protocol P8a where A == Never {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
}
protocol P8b where A == Bool {
  associatedtype A
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P8a: {
  // CHECK-NEXT: A => (ambiguous),
  // CHECK-NEXT: }
  struct Conformer: P8a, P8b {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P8a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P8b'}}
  // expected-note@-3 {{add stubs for conformance}}
}

protocol P9a where A == Never {
  associatedtype A
}
protocol P9b: P9a {
  associatedtype A
}

struct S9a: P9b {}
// expected-error@+2 {{type 'S9b' does not conform to protocol 'P9a'}}
// expected-error@+1 {{'P9a' requires the types 'S9b.A' (aka 'Bool') and 'Never' be equivalent}}
struct S9b: P9b { // expected-note {{requirement specified as 'Self.A' == 'Never' [with Self = S9b]}}
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
  associatedtype A
}
do {
  struct Conformer: Q11, P11b {}
}

protocol P12 where A == B {
  associatedtype A
  associatedtype B
  func foo(arg: A)
}
// CHECK-LABEL: Abstract type witness system for conformance of S12 to P12: {
// CHECK-NEXT: B => Self.A (preferred),
// CHECK-NEXT: }
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
// CHECK-LABEL: Abstract type witness system for conformance of S13 to P13b: {
// CHECK-NEXT: B => Self.A,
// CHECK-NEXT: }
struct S13: P13c {
  func foo(arg: Never) {}
}

protocol P14 {
  associatedtype A = Array<Self>
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Outer<Element>.Conformer to P14: {
  // CHECK-NEXT: A => Array<Self> (preferred),
  // CHECK-NEXT: }
  struct Outer<Element> {
    struct Conformer: P14 {}
  }
}

protocol P15a {
  associatedtype A
  associatedtype B = Never
}
protocol P15b: P15a where A == B {}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P15a: {
  // CHECK-NEXT: A => Never (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Never (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer: P15b {}
}

protocol P16a where A == B {
  associatedtype A
  associatedtype B = Never
}
protocol P16b: P16a {}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P16a: {
  // CHECK-NEXT: A => Never (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Never (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer: P16b {}
}

protocol P17a where A == Never {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol P17b {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1 to P17a: {
  // CHECK-NEXT: B => (unresolved){{$}}
  // CHECK-NEXT: }
  struct Conformer1: P17a {} // expected-error {{type 'Conformer1' does not conform to protocol 'P17a'}} expected-note {{add stubs for conformance}}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<A> to P17b: {
  // CHECK-NEXT: A => A (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (unresolved)
  // CHECK-NEXT: }
  struct Conformer2<A>: P17b {} // expected-error {{type 'Conformer2<A>' does not conform to protocol 'P17b'}} expected-note {{add stubs for conformance}}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer3 to P17c: {
  // CHECK-NEXT: B => Self.A (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: }
  struct Conformer3: P17c {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer4<A> to P17d: {
  // CHECK-NEXT: A => A (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: }
  struct Conformer4<A>: P17d {}
}

protocol P18 {
  associatedtype A = B
  associatedtype B = C
  associatedtype C = (D) -> D
  associatedtype D
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<D> to P18: {
  // CHECK-NEXT: A => (Self.D) -> Self.D (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (Self.D) -> Self.D (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: C => (Self.D) -> Self.D (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: D => D (preferred),
  // CHECK-NEXT: }
  struct Conformer<D>: P18 {}
}

protocol P19 where Self == A {
  associatedtype A
  associatedtype B = (A, A)
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P19: {
  // CHECK-NEXT: A => Self (preferred),
  // CHECK-NEXT: B => (Self.A, Self.A) (preferred),
  // CHECK-NEXT: }
  struct Conformer: P19 {}
}

protocol P20 where A == B.Element, B == B.SubSequence, C.Element == B.Element {
  associatedtype A
  associatedtype B: Collection
  associatedtype C: Collection = Array<Character>
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P20: {
  // CHECK-NEXT: A => Self.B.Element (preferred),
  // CHECK-NEXT: C => Array<Character> (preferred),
  // CHECK-NEXT: }
  struct Conformer: P20 {
    typealias B = Substring
  }
}

protocol P21 where A == B {
  associatedtype A
  associatedtype B = C
  associatedtype C
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<C> to P21: {
  // CHECK-NEXT: A => C (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => C (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: C => C (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer<C>: P21 {}
}

protocol P22 where A == B, C == D {
  associatedtype A
  associatedtype B
  associatedtype C = B
  associatedtype D
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<A> to P22: {
  // CHECK-NEXT: A => A (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => A (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: C => A (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: D => A (preferred), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer<A>: P22 {}
}

protocol P23 {
  associatedtype A: P23 = B.A // expected-note 2 {{protocol requires nested type 'A'}}
  associatedtype B: P23 = A.B // expected-note 2 {{protocol requires nested type 'B'}}
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P23: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: B => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer: P23 {} // expected-error {{type 'Conformer' does not conform to protocol 'P23'}}
  // CHECK-LABEL: Abstract type witness system for conformance of ConformerGeneric<T> to P23: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: B => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct ConformerGeneric<T>: P23 {} // expected-error {{type 'ConformerGeneric<T>' does not conform to protocol 'P23'}}
}

protocol P24 where A == B.A {
  associatedtype A: P24 // expected-note 2 {{protocol requires nested type 'A'}}
  associatedtype B: P24 = A.B // expected-note 2 {{protocol requires nested type 'B'}}
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P24: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: B => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer: P24 {} // expected-error {{type 'Conformer' does not conform to protocol 'P24'}} expected-note {{add stubs for conformance}}
  // CHECK-LABEL: Abstract type witness system for conformance of ConformerGeneric<T> to P24: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: B => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct ConformerGeneric<T>: P24 {} // expected-error {{type 'ConformerGeneric<T>' does not conform to protocol 'P24'}} expected-note {{add stubs for conformance}}
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<C> to P25a_1: {
  // CHECK-NEXT: B => Self.C.Element (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: C => C (preferred),
  // CHECK-NEXT: }
  struct Conformer1<C: Sequence>: P25c_1 where C.Element == Int {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<C> to P25a_2: {
  // CHECK-NEXT: B => Int (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: C => C (preferred),
  // CHECK-NEXT: }
  struct Conformer2<C: Sequence>: P25c_2 where C.Element == Int {}
}

protocol P26 where C == B, F == G {
  associatedtype A = Int
  associatedtype B = A
  associatedtype C

  associatedtype D
  associatedtype E = D

  associatedtype F
  associatedtype G = [B]
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<D> to P26: {
  // CHECK-NEXT: A => Int (preferred), [[EQUIV_CLASS_1:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int (preferred), [[EQUIV_CLASS_1]]
  // CHECK-NEXT: C => Int (preferred), [[EQUIV_CLASS_1]]
  // CHECK-NEXT: D => D (preferred), [[EQUIV_CLASS_2:0x[0-9a-f]+]]
  // CHECK-NEXT: E => D (preferred), [[EQUIV_CLASS_2]]
  // CHECK-NEXT: F => [Self.B] (preferred), [[EQUIV_CLASS_3:0x[0-9a-f]+]]
  // CHECK-NEXT: G => [Self.B] (preferred), [[EQUIV_CLASS_3]]
  // CHECK-NEXT: }
  struct Conformer<D>: P26 {}
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<B> to P27b: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: }
  struct Conformer1<B: Sequence>: P27c_1 where B.Element == Int {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<B> to P27b: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: }
  struct Conformer2<B: Sequence>: P27c_2 where B.Element == Int {}
}

protocol P28a where A == Int {
  associatedtype A // expected-note 2 {{protocol requires nested type 'A'}}
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1 to P28a: {
  // CHECK-NEXT: A => (ambiguous),
  // CHECK-NEXT: }
  struct Conformer1: Q28a {}
  // expected-error@-1 {{type 'Conformer1' does not conform to protocol 'P28a'}}
  // expected-error@-2 {{type 'Conformer1' does not conform to protocol 'P28b'}}
  // expected-note@-3 {{add stubs for conformance}}

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2 to P28a: {
  // CHECK-NEXT: A => (ambiguous),
  // CHECK-NEXT: }
  struct Conformer2: Q28b {}
  // expected-error@-1 {{type 'Conformer2' does not conform to protocol 'P28a'}}
  // expected-error@-2 {{type 'Conformer2' does not conform to protocol 'P28b'}}
  // expected-error@-3 {{type 'Conformer2' does not conform to protocol 'P28c'}}
  // expected-note@-4 {{add stubs for conformance}}
}

protocol P29a where A == Int {
  associatedtype A
  associatedtype B
}
protocol P29b where B == Never {
  associatedtype B
}
protocol P29c where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol Q29a: P29a, P29b, P29c {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
protocol Q29b: P29c, P29a, P29b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1 to P29a: {
  // CHECK-NEXT: B => Never, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: }
  struct Conformer1: Q29a {}
  // expected-note@-1 {{requirement specified as 'Self.A' == 'Self.B' [with Self = Conformer1]}}
  // expected-error@-2 {{'P29c' requires the types 'Conformer1.A' (aka 'Int') and 'Conformer1.B' (aka 'Never') be equivalent}}
  // expected-error@-3 {{type 'Conformer1' does not conform to protocol 'P29c'}}

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2 to P29c: {
  // CHECK-NEXT: B => Never (preferred), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: }
  struct Conformer2: Q29b {}
  // expected-error@-1 {{type 'Conformer2' does not conform to protocol 'P29a'}}
  // expected-error@-2 {{type 'Conformer2' does not conform to protocol 'P29b'}}
  // expected-error@-3 {{type 'Conformer2' does not conform to protocol 'P29c'}}
  // expected-note@-4 {{add stubs for conformance}}
}

protocol P30a where A == Int {
  associatedtype A
}
protocol P30b where A == Never {
  associatedtype A
}
protocol P30c where A == B {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol Q30: P30c, P30a, P30b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P30c: {
  // CHECK-NEXT: A => (ambiguous), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (ambiguous), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer: Q30 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P30a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P30b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P30c'}}
  // expected-note@-4 {{add stubs for conformance}}
}

protocol P31a where B == Int {
  associatedtype B
}
protocol P31b where B == Never {
  associatedtype B
}
protocol P31c where B == A {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol Q31: P31c, P31a, P31b {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P31c: {
  // CHECK-NEXT: B => (ambiguous), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: }
  struct Conformer: Q31 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P31a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P31b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P31c'}}
  // expected-note@-4 {{add stubs for conformance}}
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
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  associatedtype B // expected-note {{protocol requires nested type 'B'}}
}
protocol Q32: P32e, P32a, P32b, P32c, P32d {}
// expected-error@-1 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == ()'}}
// expected-error@-2 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Int'}}
// expected-error@-3 {{no type for 'Self.A' can satisfy both 'Self.A == Never' and 'Self.A == Bool'}}
// expected-error@-4 {{no type for 'Self.A' can satisfy both 'Self.A == ()' and 'Self.A == Bool'}}
// expected-error@-5 {{no type for 'Self.A' can satisfy both 'Self.A == ()' and 'Self.A == Int'}}
// expected-error@-6 {{no type for 'Self.A' can satisfy both 'Self.A == Bool' and 'Self.A == Int'}}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P32e: {
  // CHECK-NEXT: A => (ambiguous), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (ambiguous), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer: Q32 {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P32a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P32b'}}
  // expected-error@-3 {{type 'Conformer' does not conform to protocol 'P32c'}}
  // expected-error@-4 {{type 'Conformer' does not conform to protocol 'P32d'}}
  // expected-error@-5 {{type 'Conformer' does not conform to protocol 'P32e'}}
 // expected-note@-6 {{add stubs for conformance}}
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
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
}
do {
  // FIXME: should really be ambiguous (source-breaking)?
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1 to P34a: {
  // CHECK-NEXT: A => Void (preferred),
  // CHECK-NEXT: }
  struct Conformer1: Q34a {}

  // FIXME: should really be ambiguous (source-breaking)?
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2 to P34b: {
  // CHECK-NEXT: A => Never (preferred),
  // CHECK-NEXT: }
  struct Conformer2: Q34b {}

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer3 to Q34c: {
  // CHECK-NEXT: A => (unresolved){{$}}
  // CHECK-NEXT: }
  struct Conformer3: Q34c {} // expected-error {{type 'Conformer3' does not conform to protocol 'Q34c'}} expected-note {{add stubs for conformance}}
}

protocol P35 {
  associatedtype A
  associatedtype B = Array<C>
  associatedtype C = Array<D>
  associatedtype D = Array<A>
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P35: {
  // CHECK-NEXT: B => Array<Self.C> (preferred),
  // CHECK-NEXT: C => Array<Self.D> (preferred),
  // CHECK-NEXT: D => Array<Self.A> (preferred),
  // CHECK-NEXT: }
  struct Conformer: P35 {
    typealias A = Never
  }
  // CHECK-LABEL: Abstract type witness system for conformance of ConformerGeneric<A> to P35: {
  // CHECK-NEXT: A => A (preferred),
  // CHECK-NEXT: B => Array<Self.C> (preferred),
  // CHECK-NEXT: C => Array<Self.D> (preferred),
  // CHECK-NEXT: D => Array<Self.A> (preferred),
  // CHECK-NEXT: }
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P36: {
  // CHECK-NEXT: A => G36<Self.B> (preferred),
  // CHECK-NEXT: B => Never (preferred),
  // CHECK-NEXT: }
  struct Conformer: P36 {} // expected-error {{type 'Conformer' does not conform to protocol 'P36'}}
}

protocol P37a {
  associatedtype A
}
protocol P37b {
  associatedtype B : P37a
  associatedtype C where C == B.A
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<C> to P37b: {
  // CHECK-NEXT: C => Self.B.A (preferred),
  // CHECK-NEXT: }
  struct Conformer1<C>: P37b {
    struct Inner: P37a { typealias A = C }

    typealias B = Inner
  }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<T> to P37b: {
  // CHECK-NEXT: C => Self.B.A (preferred),
  // CHECK-NEXT: }
  struct Conformer2<T>: P37b {
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<T> to P38b: {
  // CHECK-NEXT: B => Self.A,
  // CHECK-NEXT: }
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P39: {
  // CHECK-NEXT: B => Self.A.C (preferred),
  // CHECK-NEXT: C => Never (preferred),
  // CHECK-NEXT: }
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P40c: {
  // CHECK-NEXT: A => Int,
  // CHECK-NEXT: B => (Self.A, Self.A),
  // CHECK-NEXT: }
  struct Conformer: P40c {}
}

protocol P41 {
  associatedtype A where A == B.A // expected-note {{protocol requires nested type 'A'}}
  associatedtype B: P41 = Self // expected-note {{protocol requires nested type 'B'}}
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P41: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: B => Self (preferred),
  // CHECK-NEXT: }
  struct Conformer: P41 {} // expected-error{{type 'Conformer' does not conform to protocol 'P41'}} expected-note {{add stubs for conformance}}
}

protocol P42a {
  associatedtype B: P42b
}
protocol P42b: P42a {
  associatedtype A = B.A
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<B> to P42b: {
  // CHECK-NEXT: A => Self.B.A (preferred),
  // CHECK-NEXT: }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<B> to P42a: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: }
  struct Conformer<B: P42b>: P42b {}
}

protocol P43a {
  associatedtype A: P43a
  associatedtype B
}
protocol P43b: P43a {
  associatedtype C where C == A.B
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<B> to P43b: {
  // CHECK-NEXT: C => Self.A.B (preferred),
  // CHECK-NEXT: }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<B> to P43a: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: }
  struct Conformer<B: P43a>: P43b {
    typealias A = Conformer<B.A>
  }
}

protocol P44 {
  associatedtype A: P44
  associatedtype B
  associatedtype C where C == A.B
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<T> to P44: {
  // CHECK-NEXT: C => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer1<T: P44>: P44 {
    typealias B = T.A
    typealias A = Conformer1<T.A>
  }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<B> to P44: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: C => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer2<B: P44>: P44 {
    typealias A = Conformer2<B.A>
  }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer3<B> to P44: {
  // CHECK-NEXT: B => B (preferred),
  // CHECK-NEXT: C => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer3<B>: P44 {
    typealias A = Conformer3<Int>
  }
}

protocol P45 {
  associatedtype A
  associatedtype B: P45 = Conformer45<D>
  associatedtype C where C == B.A
  associatedtype D = Never
}
// CHECK-LABEL: Abstract type witness system for conformance of Conformer45<A> to P45: {
// CHECK-NEXT: A => A (preferred),
// CHECK-NEXT: B => Conformer45<Self.D> (preferred),
// CHECK-NEXT: C => Self.B.A (preferred),
// CHECK-NEXT: D => Never (preferred),
// CHECK-NEXT: }
struct Conformer45<A>: P45 {}

protocol P46 {
  associatedtype A: P46
  associatedtype B
  associatedtype C where C == A.B

  func method(_: B)
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer<T> to P46: {
  // CHECK-NEXT: C => Self.A.B (preferred),
  // CHECK-NEXT: }
  struct Conformer<T: P46>: P46 {
    typealias A = Conformer<T.A>

    func method(_: T) {}
  }
}


protocol P47 {
  associatedtype A
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Outer<A>.Inner to P47: {
  // CHECK-NEXT: A => A (preferred),
  // CHECK-NEXT: }
  struct Outer<A> {
    struct Inner: P47 {}
  }
}

protocol P48a { associatedtype A = Int } // expected-note {{protocol requires nested type 'A'}}
protocol P48b { associatedtype B } // expected-note {{protocol requires nested type 'B'}}
protocol P48c: P48a, P48b where A == B {}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P48a: {
  // CHECK-NEXT: A => Self.B,
  // CHECK-NEXT: }

  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P48b: {
  // CHECK-NEXT: B => Self.A,
  // CHECK-NEXT: }

  // CHECK-NOT: Abstract type witness system for conformance of Conformer to P48c

  // FIXME: Should compile
  struct Conformer: P48c {}
  // expected-error@-1 {{type 'Conformer' does not conform to protocol 'P48a'}}
  // expected-error@-2 {{type 'Conformer' does not conform to protocol 'P48b'}}
  // expected-note@-3 {{add stubs for conformance}}
}
