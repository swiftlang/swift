// RUN: %target-typecheck-verify-swift -warn-redundant-requirements

// Reduced from https://github.com/plx/HDXLSIMDSupport.

protocol PA: PB, PC, PD {}

protocol PB: PE where A2 == A3, A4 == A2, A5 == A6, A1 == Self {}
// expected-warning@-1 {{redundant same-type constraint 'Self.A2' == 'Self.A3'}}
// expected-warning@-2 {{redundant same-type constraint 'Self.A5' == 'Self.A6'}}

protocol PC: PG where A5 == (A2, A2), A3 == A7.A8 {}

protocol PD: PG where A6 == (A3, A3), A2 == A7.A8 {}

protocol PE: PF where A1.A1 == Self {}

protocol PF: PG {
  associatedtype A1: PF where A1.A7 == A7, A1.A2 == A3, A1.A3 == A2, A1.A5 == A6, A1.A6 == A5
  // expected-warning@-1 {{redundant same-type constraint 'Self.A1.A7' == 'Self.A7'}}
}

protocol PG: PH where A7: PI, A7: BinaryFloatingPoint {
  associatedtype A2: PJ where A2.A7 == A7
  associatedtype A3: PJ where A3.A7 == A7
  associatedtype A4: PJ where A4.A7 == A7
  associatedtype A5
  associatedtype A6
}

protocol PH {
  associatedtype A7: SIMDScalar
}

protocol PI {
// expected-warning@-1 {{protocol 'PI' should be declared to refine 'Decodable' due to a same-type constraint on 'Self'}}
// expected-warning@-2 {{protocol 'PI' should be declared to refine 'Encodable' due to a same-type constraint on 'Self'}}
// expected-warning@-3 {{protocol 'PI' should be declared to refine 'Hashable' due to a same-type constraint on 'Self'}}
// expected-warning@-4 {{protocol 'PI' should be declared to refine 'SIMDScalar' due to a same-type constraint on 'Self'}}
  associatedtype A8 where A8.A7 == Self // expected-warning {{redundant same-type constraint 'Self.A8.A7' == 'Self'}}
  associatedtype A9 where A9.A7 == Self, A9.A8 == A8
  associatedtype A10 where A10.A7 == Self, A10.A8 == A8, A10.A9 == A9 // expected-warning {{redundant same-type constraint 'Self.A10.A7' == 'Self'}}
  associatedtype A11 where A11.A7 == Self, A11.A10 == A10 // expected-warning {{redundant same-type constraint 'Self.A11.A7' == 'Self'}}
  associatedtype A12 where A12.A7 == Self, A12.A11 == A11 // expected-warning {{redundant same-type constraint 'Self.A12.A7' == 'Self'}}
  associatedtype A13 where A13.A7 == Self, A13.A12 == A12 // expected-warning {{redundant same-type constraint 'Self.A13.A7' == 'Self'}}
  associatedtype A14: PK where A14.A7 == Self, A14.A13 == A13 // expected-warning {{redundant same-type constraint 'Self.A14.A7' == 'Self'}}
}

protocol PJ: SIMD, PH where Scalar == A7 {}

protocol PK: SIMD, PH where Scalar == A7 {
  associatedtype A13: PL where A13.Scalar == Scalar
}

protocol PL: SIMD, PH where Scalar == A7 {
  associatedtype A12: PM where A12.Scalar == Scalar
}

protocol PM: SIMD, PH where Scalar == A7 {
  associatedtype A11: PN where A11.Scalar == Scalar
}

protocol PN: SIMD, PH where Scalar == A7 {
  associatedtype A10: PO where A10.Scalar == Scalar
}

protocol PO: SIMD, PH where Scalar == A7 {
  associatedtype A8: PQ where A8.Scalar == Scalar
  associatedtype A9: PP where A9.Scalar == Scalar
}

protocol PP: SIMD, PH where Scalar == A7 {
  associatedtype A8: PQ where A8.Scalar == Scalar
}

protocol PQ: SIMD, PH where Scalar == A7 {}

func sameType<T>(_: T, _: T) {}

func testPI1<T : PI>(_: T) {
  sameType(T.A8.A7.self, T.self)
}

func testPI2<T : PI>(_: T) {
  sameType(T.A10.A7.self, T.self)
}

func testPI3<T : PI>(_: T) {
  sameType(T.A11.A7.self, T.self)
}

func testPI4<T : PI>(_: T) {
  sameType(T.A12.A7.self, T.self)
}

func testPI5<T : PI>(_: T) {
  sameType(T.A13.A7.self, T.self)
}

func testPI6<T : PI>(_: T) {
  sameType(T.A14.A7.self, T.self)
}

func testPF1<T : PF>(_: T) {
  sameType(T.A1.A7.self, T.A7.self)
}

func testPB1<T : PB>(_: T) {
  sameType(T.A2.self, T.A3.self)
}

func testPB2<T : PB>(_: T) {
  sameType(T.A5.self, T.A6.self)
}
