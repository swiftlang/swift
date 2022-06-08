// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: rdar91232987.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]A1 : Decodable, Self.[P1]A1 : Encodable, Self.[P1]A1 : P2, Self.[P1]A1.[P2]A3.[P5]A6 : Decodable, Self.[P1]A1.[P2]A3.[P5]A6 : Encodable>
protocol P1 {
  associatedtype A1: P2 where A1: Codable, A1.A3.A6: Codable
}

// CHECK-LABEL: rdar91232987.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self : P3, Self.[P2]A2 : P4, Self.[P2]A3 : P5>
protocol P2: P3 {
  associatedtype A2: P4
  associatedtype A3: P5
}

// CHECK-LABEL: rdar91232987.(file).P3@
// CHECK-NEXT: Requirement signature: <Self>
protocol P3 {}

// CHECK-LABEL: rdar91232987.(file).P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]A4 : P11, Self.[P4]A5 : P9>
protocol P4 {
  associatedtype A4: P11
  associatedtype A5: P9

}

// CHECK-LABEL: rdar91232987.(file).P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]A6 : P6>
protocol P5 {
  associatedtype A6: P6
}

// CHECK-LABEL: rdar91232987.(file).P6@
// CHECK-NEXT: Requirement signature: <Self where Self : P10, Self : P7, Self.[P6]A7 : Decodable, Self.[P6]A7 : Encodable, Self.[P6]A7 : Hashable, Self.[P6]A7 : RawRepresentable, Self.[P6]A7.[RawRepresentable]RawValue == String>
protocol P6: P7, P10 {
  associatedtype A7: Hashable, Codable, RawRepresentable where A7.RawValue == String
}

// CHECK-LABEL: rdar91232987.(file).P7@
// CHECK-NEXT: Requirement signature: <Self where Self : Equatable, Self : P8>
protocol P7: Equatable, P8 {}

// CHECK-LABEL: rdar91232987.(file).P8@
// CHECK-NEXT: Requirement signature: <Self>
protocol P8 {}

// CHECK-LABEL: rdar91232987.(file).P9@
// CHECK-NEXT: Requirement signature: <Self>
protocol P9 {
  associatedtype A8
}

// CHECK-LABEL: rdar91232987.(file).P10@
// CHECK-NEXT: Requirement signature: <Self>
protocol P10 {}

// CHECK-LABEL: rdar91232987.(file).P11@
// CHECK-NEXT: Requirement signature: <Self where Self.[P11]A10 : P13, Self.[P11]A9 : P12>
protocol P11 {
  associatedtype A9: P12
  associatedtype A10: P13
}

// CHECK-LABEL: rdar91232987.(file).P12@
// CHECK-NEXT: Requirement signature: <Self where Self : Decodable, Self : Encodable, Self : Equatable>
protocol P12: Codable, Equatable {}

// CHECK-LABEL: rdar91232987.(file).P13@
// CHECK-NEXT: Requirement signature: <Self>
protocol P13 {}

// CHECK-LABEL: rdar91232987.(file).P14@
// CHECK-NEXT: Requirement signature: <Self where Self : P16, Self.[P14]A1 : P2>
protocol P14: P16 {
  associatedtype A1: P2
}

// CHECK-LABEL: rdar91232987.(file).P15@
// CHECK-NEXT: Requirement signature: <Self where Self.[P15]A6 : P6>
protocol P15 {
  associatedtype A6: P6
}

// CHECK-LABEL: rdar91232987.(file).P16@
// CHECK-NEXT: Requirement signature: <Self>
protocol P16 {}

// CHECK-LABEL: rdar91232987.(file).C@
// CHECK-NEXT: Generic signature: <X, Y where X : P14, Y : P15, X.[P14]A1 : Decodable, X.[P14]A1 : Encodable, Y.[P15]A6 : Decodable, Y.[P15]A6 : Encodable, Y.[P15]A6 == X.[P14]A1.[P2]A3.[P5]A6>
class C<X: P14, Y: P15> where X.A1: Codable, X.A1.A3.A6: Codable, X.A1.A3.A6 == Y.A6 {}
