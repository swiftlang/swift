// RUN: %target-typecheck-verify-swift -swift-version 4

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

protocol P5 : P4 {
	associatedtype AssocP5 where AssocP3: Q0
}

func acceptP0<T: P0>(_: T) { }
func acceptQ0<T: Q0>(_: T) { }
func acceptP1<T: P1>(_: T) { }
func acceptP2<T: P2>(_: T) { }
func acceptP3<T: P3>(_: T) { }


func testPaths1<T: P2 & P4>(_ t: T) {
	acceptP0(t.getAssocP2().getAssocP1())
	acceptP0(t.getAssocP3())
}

func testPaths2<U: P2 & P4>(_ t: U) where U.AssocP3 == U.AssocP2.AssocP1 {
	acceptP0(t.getAssocP2().getAssocP1())
}

func testPaths3<V: P5>(_ v: V) {
	acceptP0(v.getAssocP3())
	acceptQ0(v.getAssocP3())
}

protocol P6Unordered: P5Unordered { // expected-note{{conformance constraint 'Self.A': 'P0' implied here}}
	associatedtype A: P0 // expected-warning{{redundant conformance constraint 'Self.A': 'P0'}}
                       // expected-warning@-1{{redeclaration of associated type 'A'}}
}

protocol P5Unordered {
	associatedtype A: P0 // expected-note{{'A' declared here}}

	func getA() -> A
}

func testUnorderedP5_P6<W: P6Unordered>(_ w: W) {
	acceptP0(w.getA())
}
