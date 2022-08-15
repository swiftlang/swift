// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A
}

protocol P2 {
  associatedtype B
}

// CHECK-LABEL: protocol_typealias_same_type_requirement.(file).P3@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self : P2, Self.[P1]A == Self.[P2]B>
protocol P3 : P1, P2 {
  typealias A = B
}

// CHECK-LABEL: protocol_typealias_same_type_requirement.(file).P4@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self : P2, Self.[P2]B == Int>
protocol P4 : P1, P2 {
  typealias B = Int
}

// CHECK-LABEL: protocol_typealias_same_type_requirement.(file).P5@
// CHECK-LABEL: Requirement signature: <Self>
protocol P5 {
  associatedtype A
  associatedtype B
}

extension P5 where A == Int {
  typealias B = Int
}

protocol P6 {
  typealias A = Array<Int>
}

protocol P7 {
  associatedtype X
  typealias A = Array<X>
}

// CHECK-LABEL: protocol_typealias_same_type_requirement.(file).P8@
// CHECK-LABEL: Requirement signature: <Self where Self : P6, Self : P7, Self.[P7]X == Int>
protocol P8 : P6, P7 {}

func testP8<T : P8>(_: T, x: T.X) -> Int { return x }
