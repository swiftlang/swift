// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -typecheck -dump-type-witness-systems %s 2>&1 | %FileCheck %s

protocol P1 where A == Never {
  associatedtype A
}
// CHECK-LABEL: Abstract type witness system for conformance of S1 to P1: {
// CHECK-NEXT: A => Never,
// CHECK-NEXT: }
struct S1: P1 {}

protocol P2a {
  associatedtype A
}
protocol P2b: P2a where A == Never {}
protocol P2c: P2b {}
// CHECK-LABEL: Abstract type witness system for conformance of S2a to P2a: {
// CHECK-NEXT: A => Never,
// CHECK-NEXT: }
struct S2a: P2b {}
// CHECK-LABEL: Abstract type witness system for conformance of S2b to P2a: {
// CHECK-NEXT: A => Never,
// CHECK-NEXT: }
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
// CHECK-NEXT: A => [Self.B],
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


protocol P6 where A == Never { // expected-error {{same-type constraint type 'Never' does not conform to required protocol 'P6'}}
  // expected-error@+2 {{same-type constraint type 'Never' does not conform to required protocol 'P6'}}
  // expected-note@+1 {{protocol requires nested type 'A}}
  associatedtype A: P6
}
// CHECK-LABEL: Abstract type witness system for conformance of S6 to P6: {
// CHECK-NEXT: A => (unresolved){{$}}
// CHECK-NEXT: }
struct S6: P6 {} // expected-error {{type 'S6' does not conform to protocol 'P6'}}

protocol P7a where A == Never {
  associatedtype A
}
// expected-error@+2 {{'Self.A' cannot be equal to both 'Bool' and 'Never'}}
// expected-note@+1 {{same-type constraint 'Self.A' == 'Never' implied here}}
protocol P7b: P7a where A == Bool {}
// CHECK-LABEL: Abstract type witness system for conformance of S7 to P7a: {
// CHECK-NEXT: A => Never,
// CHECK-NEXT: }
struct S7: P7b {}

// FIXME: Handle ambiguities.
//protocol P8a where A == Never {
//  associatedtype A
//}
//protocol P8b where A == Bool {
//  associatedtype A
//}
//struct S8: P8a, P8b {}

protocol P9a where A == Never {
  associatedtype A
}
protocol P9b: P9a {
  associatedtype A
}
// CHECK-LABEL: Abstract type witness system for conformance of S9a to P9b: {
// CHECK-NEXT: A => Never,
// CHECK-NEXT: }
struct S9a: P9b {}
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to Q11: {
  // CHECK-NEXT: A => Never,
  // CHECK-NEXT: }
  struct Conformer: Q11, P11b {}
}

protocol P12 where A == B {
  associatedtype A
  associatedtype B
  func foo(arg: A)
}
// CHECK-LABEL: Abstract type witness system for conformance of S12 to P12: {
// CHECK-NEXT: B => Self.A,
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
  // CHECK-NEXT: A => Array<Self>,
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
  // CHECK-NEXT: A => Never, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Never, [[EQUIV_CLASS]]
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
  // CHECK-NEXT: A => Never, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Never, [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer: P16b {}
}

protocol P17a where A == Never {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; do you want to add it?}}
}
protocol P17b {
  associatedtype A = B // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
  associatedtype B // expected-note {{protocol requires nested type 'B'; do you want to add it?}}
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
  // CHECK-NEXT: A => Never,
  // CHECK-NEXT: B => (unresolved){{$}}
  // CHECK-NEXT: }
  struct Conformer1: P17a {} // expected-error {{type 'Conformer1' does not conform to protocol 'P17a'}}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<A> to P17b: {
  // CHECK-NEXT: A => (unresolved), [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (unresolved), [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer2<A>: P17b {} // expected-error {{type 'Conformer2<A>' does not conform to protocol 'P17b'}}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer3 to P17c: {
  // CHECK-NEXT: A => Never, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Never, [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer3: P17c {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer4<A> to P17d: {
  // CHECK-NEXT: A => Int, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int, [[EQUIV_CLASS]]
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
  // CHECK-NEXT: A => (Self.D) -> Self.D, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => (Self.D) -> Self.D, [[EQUIV_CLASS]]
  // CHECK-NEXT: C => (Self.D) -> Self.D, [[EQUIV_CLASS]]
  // CHECK-NEXT: D => D,
  // CHECK-NEXT: }
  struct Conformer<D>: P18 {}
}

protocol P19 where Self == A {
  associatedtype A
  associatedtype B = (A, A)
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P19: {
  // CHECK-NEXT: A => Self,
  // CHECK-NEXT: B => (Self.A, Self.A),
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
  // CHECK-NEXT: A => Self.B.Element,
  // CHECK-NEXT: C => Array<Character>,
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
  // CHECK-NEXT: A => C, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => C, [[EQUIV_CLASS]]
  // CHECK-NEXT: C => C, [[EQUIV_CLASS]]
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
  // CHECK-NEXT: A => A, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => A, [[EQUIV_CLASS]]
  // CHECK-NEXT: C => A, [[EQUIV_CLASS]]
  // CHECK-NEXT: D => A, [[EQUIV_CLASS]]
  // CHECK-NEXT: }
  struct Conformer<A>: P22 {}
}

protocol P23 {
  associatedtype A: P23 = B.A // expected-note 2 {{protocol requires nested type 'A'; do you want to add it?}}
  associatedtype B: P23 = A.B // expected-note 2 {{protocol requires nested type 'B'; do you want to add it?}}
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P23: {
  // CHECK-NEXT: A => Self.B.A,
  // CHECK-NEXT: B => Self.A.B,
  // CHECK-NEXT: }
  struct Conformer: P23 {} // expected-error {{type 'Conformer' does not conform to protocol 'P23'}}
  // CHECK-LABEL: Abstract type witness system for conformance of ConformerGeneric<T> to P23: {
  // CHECK-NEXT: A => Self.B.A,
  // CHECK-NEXT: B => Self.A.B,
  // CHECK-NEXT: }
  struct ConformerGeneric<T>: P23 {} // expected-error {{type 'ConformerGeneric<T>' does not conform to protocol 'P23'}}
}

protocol P24 where A == B.A {
  associatedtype A: P24 // expected-note 2 {{protocol requires nested type 'A'; do you want to add it?}}
  associatedtype B: P24 = A.B // expected-note 2 {{protocol requires nested type 'B'; do you want to add it?}}
}
do {
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer to P24: {
  // CHECK-NEXT: A => Self.B.A,
  // CHECK-NEXT: B => Self.A.B,
  // CHECK-NEXT: }
  struct Conformer: P24 {} // expected-error {{type 'Conformer' does not conform to protocol 'P24'}}
  // CHECK-LABEL: Abstract type witness system for conformance of ConformerGeneric<T> to P24: {
  // CHECK-NEXT: A => Self.B.A,
  // CHECK-NEXT: B => Self.A.B,
  // CHECK-NEXT: }
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<C> to P25a_1: {
  // CHECK-NEXT: A => Int, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int, [[EQUIV_CLASS]]
  // CHECK-NEXT: C => C,
  // CHECK-NEXT: }
  struct Conformer1<C: Sequence>: P25c_1 where C.Element == Int {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<C> to P25a_2: {
  // CHECK-NEXT: A => Int, [[EQUIV_CLASS:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int, [[EQUIV_CLASS]]
  // CHECK-NEXT: C => C,
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
  // CHECK-NEXT: A => Int, [[EQUIV_CLASS_1:0x[0-9a-f]+]]
  // CHECK-NEXT: B => Int, [[EQUIV_CLASS_1]]
  // CHECK-NEXT: C => Int, [[EQUIV_CLASS_1]]
  // CHECK-NEXT: D => D, [[EQUIV_CLASS_2:0x[0-9a-f]+]]
  // CHECK-NEXT: E => D, [[EQUIV_CLASS_2]]
  // CHECK-NEXT: F => [Self.B], [[EQUIV_CLASS_3:0x[0-9a-f]+]]
  // CHECK-NEXT: G => [Self.B], [[EQUIV_CLASS_3]]
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
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<B> to P27a: {
  // CHECK-NEXT: A => Int,
  // CHECK-NEXT: }
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer1<B> to P27b: {
  // CHECK-NEXT: B => B,
  // CHECK-NEXT: }
  struct Conformer1<B: Sequence>: P27c_1 where B.Element == Int {}
  // CHECK-LABEL: Abstract type witness system for conformance of Conformer2<B> to P27b: {
  // CHECK-NEXT: A => Int,
  // CHECK-NEXT: B => B,
  // CHECK-NEXT: }
  struct Conformer2<B: Sequence>: P27c_2 where B.Element == Int {}
}

// FIXME: Handle ambiguities.
//protocol P28a where A == Int {
//  associatedtype A
//}
//protocol P28b where A == Bool {
//  associatedtype A
//}
//protocol P28c where A == Never {
//  associatedtype A
//}
//protocol Q28a: P28a, P28b {}
//protocol Q28b: P28a, P28b, P28c {}
//do {
//  struct Conformer1: Q28a {}
//  struct Conformer2: Q28b {}
//}
