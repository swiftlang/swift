// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1
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

// CHECK: P2
// CHECK: Requirement signature: <Self where Self == Self.Y.X, Self.Y : P1, Self.Z : P1>
protocol P2 {
  associatedtype Y : P1 where Y.X == Self
  associatedtype Z : P1
}

// SR-5473
protocol P3 {
	associatedtype X : P4
}

// CHECK: .P4@
// CHECK: Requirement signature: <Self where Self == Self.Y.X, Self.Y : P3, Self.Z : P3, Self.Y.X == Self.Z.X>
protocol P4 {
	associatedtype Y: P3 where Y.X == Self
	associatedtype Z: P3 where Z.X == Self
}
