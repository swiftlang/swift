// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -debug-generic-signatures > %t.dump 2>&1
// RUN: %FileCheck %s < %t.dump

protocol P {
  associatedtype Assoc : P

  var assoc: Assoc { get }
}

func testP<T: P>(_ t: T) {
  testP(t.assoc)
  testP(t.assoc.assoc)
  testP(t.assoc.assoc.assoc)
  testP(t.assoc.assoc.assoc.assoc.assoc.assoc.assoc)
}

// SR-5485
protocol P1 {
  associatedtype X : P2
}

// CHECK-LABEL: .P2@
// CHECK: Requirement signature: <Self where Self == Self.Y.X, Self.Y : P1, Self.Z : P1>
protocol P2 {
  associatedtype Y : P1 where Y.X == Self
  associatedtype Z : P1
}

// SR-5473
protocol P3 {
	associatedtype X : P4
}

// CHECK-LABEL: .P4@
// CHECK: Requirement signature: <Self where Self == Self.Y.X, Self.Y : P3, Self.Z : P3, Self.Y.X == Self.Z.X>
protocol P4 {
	associatedtype Y: P3 where Y.X == Self
	associatedtype Z: P3 where Z.X == Self
}

protocol P5 {
  associatedtype X : P5
    where X.X == X
}

// CHECK-LABEL: .P6@
// CHECK: Requirement signature: <Self where Self : P5, Self.Y : P5>
protocol P6 : P5 {
  associatedtype Y : P5
}

// CHECK: Generic signature: <Self where Self : P6, Self.X == Self.Y.X>
extension P6 where X == Y.X { }

// SR-5601
protocol P7 {
    associatedtype X: P9 where X.Q == Self, X.R == UInt8
    associatedtype Y: P9 where Y.Q == Self, Y.R == UInt16
    // NOTE: Removing either X or Y from P7 (and A7) makes the program compile.
}
struct A7: P7 {
    typealias X = S9<UInt8>
    typealias Y = S9<UInt16>
}
protocol P8 { }
protocol P9 : P8 { // NOTE: Removing ": P8 " here makes the program compile.
    associatedtype Q: P7
    associatedtype R
}
struct S9<E> : P9 {
    typealias R = E
    typealias Q = A7
}

// SR-5610
protocol P10 {
  associatedtype X : P11 where X.Q == Self
}
protocol P11 {
  associatedtype Q : P10

  // CHECK-LABEL: .P11.map@
  // CHECK: Generic signature: <Self, T where Self : P11, T : P11, Self.Q == T.Q>
  func map<T>(_: T.Type) where T : P11, Q == T.Q
}

// Redundances within a requirement signature.
protocol P12 { }

protocol P13 {
  associatedtype AT1 : P12
  associatedtype AT2: P13 where AT2.AT1 == AT1
}
