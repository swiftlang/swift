// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// Performance test case reduced from https://github.com/plx/HDXLSIMDSupport.

// CHECK-LABEL: .PA@
// CHECK-NEXT: Requirement signature: <Self where Self : PB, Self : PC, Self : PD>
protocol PA: PB, PC, PD {}

// CHECK-LABEL: .PB@
// CHECK-NEXT: Requirement signature: <Self where Self : PE, Self == Self.[PF]A1, Self.[PG]A2 == Self.[PG]A4>
protocol PB: PE where A2 == A3, A4 == A2, A5 == A6, A1 == Self {}

// CHECK-LABEL: .PC@
// CHECK-NEXT: Requirement signature: <Self where Self : PG, Self.[PG]A3 == Self.[PH]A7.[PI]A8, Self.[PG]A5 == (Self.[PG]A2, Self.[PG]A2)>
protocol PC: PG where A5 == (A2, A2), A3 == A7.A8 {}

// CHECK-LABEL: .PD@
// CHECK-NEXT: Requirement signature: <Self where Self : PG, Self.[PG]A2 == Self.[PH]A7.[PI]A8, Self.[PG]A6 == (Self.[PG]A3, Self.[PG]A3)>
protocol PD: PG where A6 == (A3, A3), A2 == A7.A8 {}

// CHECK-LABEL: .PE@
// CHECK-NEXT: Requirement signature: <Self where Self : PF, Self == Self.[PF]A1.[PF]A1>
protocol PE: PF where A1.A1 == Self {}

// CHECK-LABEL: .PF@
// CHECK-NEXT: Requirement signature: <Self where Self : PG, Self.[PF]A1 : PF, Self.[PG]A2 == Self.[PF]A1.[PG]A3, Self.[PG]A3 == Self.[PF]A1.[PG]A2, Self.[PG]A5 == Self.[PF]A1.[PG]A6, Self.[PG]A6 == Self.[PF]A1.[PG]A5>
protocol PF: PG {
  associatedtype A1: PF where A1.A7 == A7, A1.A2 == A3, A1.A3 == A2, A1.A5 == A6, A1.A6 == A5
}

// CHECK-LABEL: .PG@
// CHECK-NEXT: Requirement signature: <Self where Self : PH, Self.[PG]A2 : PJ, Self.[PG]A3 : PJ, Self.[PG]A4 : PJ, Self.[PH]A7 : BinaryFloatingPoint, Self.[PH]A7 : PI, Self.[PH]A7 == Self.[PG]A2.[PH]A7, Self.[PG]A2.[PH]A7 == Self.[PG]A3.[PH]A7, Self.[PG]A3.[PH]A7 == Self.[PG]A4.[PH]A7>
protocol PG: PH where A7: PI, A7: BinaryFloatingPoint {
  associatedtype A2: PJ where A2.A7 == A7
  associatedtype A3: PJ where A3.A7 == A7
  associatedtype A4: PJ where A4.A7 == A7
  associatedtype A5
  associatedtype A6
}

// CHECK-LABEL: .PH@
// CHECK-NEXT: Requirement signature: <Self where Self.[PH]A7 : SIMDScalar>
protocol PH {
  associatedtype A7: SIMDScalar
}

// CHECK-LABEL: .PI@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[PI]A10.[PH]A7, Self.[PI]A10 == Self.[PI]A11.[PN]A10, Self.[PI]A11 == Self.[PI]A12.[PM]A11, Self.[PI]A12 == Self.[PI]A13.[PL]A12, Self.[PI]A13 == Self.[PI]A14.[PK]A13, Self.[PI]A14 : PK, Self.[PI]A8 == Self.[PI]A10.[PO]A8, Self.[PI]A9 == Self.[PI]A10.[PO]A9, Self.[PI]A10.[PO]A8 == Self.[PI]A9.[PP]A8>
protocol PI {
// expected-warning@-1 {{protocol 'PI' should be declared to refine 'Decodable' due to a same-type constraint on 'Self'}}
// expected-warning@-2 {{protocol 'PI' should be declared to refine 'Encodable' due to a same-type constraint on 'Self'}}
// expected-warning@-3 {{protocol 'PI' should be declared to refine 'Hashable' due to a same-type constraint on 'Self'}}
// expected-warning@-4 {{protocol 'PI' should be declared to refine 'SIMDScalar' due to a same-type constraint on 'Self'}}
  associatedtype A8 where A8.A7 == Self
  associatedtype A9 where A9.A7 == Self, A9.A8 == A8
  associatedtype A10 where A10.A7 == Self, A10.A8 == A8, A10.A9 == A9
  associatedtype A11 where A11.A7 == Self, A11.A10 == A10
  associatedtype A12 where A12.A7 == Self, A12.A11 == A11
  associatedtype A13 where A13.A7 == Self, A13.A12 == A12
  associatedtype A14: PK where A14.A7 == Self, A14.A13 == A13
}

// CHECK-LABEL: .PJ@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PH]A7 == Self.[SIMDStorage]Scalar>
protocol PJ: SIMD, PH where Scalar == A7 {}

// CHECK-LABEL: .PK@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PK]A13 : PL, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[SIMDStorage]Scalar == Self.[PK]A13.[PH]A7>
protocol PK: SIMD, PH where Scalar == A7 {
  associatedtype A13: PL where A13.Scalar == Scalar
}

// CHECK-LABEL: .PL@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PL]A12 : PM, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[SIMDStorage]Scalar == Self.[PL]A12.[PH]A7>
protocol PL: SIMD, PH where Scalar == A7 {
  associatedtype A12: PM where A12.Scalar == Scalar
}

// CHECK-LABEL: .PM@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PM]A11 : PN, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[SIMDStorage]Scalar == Self.[PM]A11.[PH]A7>
protocol PM: SIMD, PH where Scalar == A7 {
  associatedtype A11: PN where A11.Scalar == Scalar
}

// CHECK-LABEL: .PN@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PN]A10 : PO, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[SIMDStorage]Scalar == Self.[PN]A10.[PH]A7>
protocol PN: SIMD, PH where Scalar == A7 {
  associatedtype A10: PO where A10.Scalar == Scalar
}

// CHECK-LABEL: .PO@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[PO]A8 : PQ, Self.[PO]A9 : PP, Self.[SIMDStorage]Scalar == Self.[PO]A8.[PH]A7, Self.[PO]A8.[PH]A7 == Self.[PO]A9.[PH]A7>
protocol PO: SIMD, PH where Scalar == A7 {
  associatedtype A8: PQ where A8.Scalar == Scalar
  associatedtype A9: PP where A9.Scalar == Scalar
}

// CHECK-LABEL: .PP@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PH]A7 == Self.[SIMDStorage]Scalar, Self.[PP]A8 : PQ, Self.[SIMDStorage]Scalar == Self.[PP]A8.[PH]A7>
protocol PP: SIMD, PH where Scalar == A7 {
  associatedtype A8: PQ where A8.Scalar == Scalar
}

// CHECK-LABEL: .PQ@
// CHECK-NEXT: Requirement signature: <Self where Self : SIMD, Self : PH, Self.[PH]A7 == Self.[SIMDStorage]Scalar>
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
