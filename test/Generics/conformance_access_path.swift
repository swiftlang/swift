// RUN: %target-typecheck-verify-swift -typecheck %s -verify
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

protocol P0 { }
protocol Q0: P0 { }

protocol P1 {
	associatedtype AssocP1: Q0

	func getAssocP1() -> AssocP1
}

protocol P2 : P1 {
	associatedtype AssocP2: P1

	func getAssocP2() -> AssocP2
}

protocol P3 {
	associatedtype AssocP3: P0

	func getAssocP3() -> AssocP3
}

protocol P4: P3 { }

func acceptP0<T: P0>(_: T) { }
func acceptP1<T: P1>(_: T) { }
func acceptP2<T: P2>(_: T) { }
func acceptP3<T: P3>(_: T) { }


func testPaths1<T: P2 & P4>(_ t: T) {
	// CHECK: Conformance access path for T.AssocP2.AssocP1: P0 is T: P2 -> τ_0_0.AssocP2: P1 -> τ_0_0.AssocP1: Q0 -> τ_0_0: P0
	acceptP0(t.getAssocP2().getAssocP1())
	// CHECK: Conformance access path for T.AssocP3: P0 is T: P4 -> τ_0_0: P3 -> τ_0_0.AssocP3: P0
	acceptP0(t.getAssocP3())
}

func testPaths2<U: P2 & P4>(_ t: U) where U.AssocP3 == U.AssocP2.AssocP1 {
	// CHECK: Conformance access path for U.AssocP3: P0 is U: P4 -> τ_0_0: P3 -> τ_0_0.AssocP3: P0
	acceptP0(t.getAssocP2().getAssocP1())
}
