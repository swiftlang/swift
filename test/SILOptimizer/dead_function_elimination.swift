// RUN: %target-swift-frontend %s -O -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-sil -enable-testing | %FileCheck -check-prefix=CHECK-TESTING %s

// Check if cycles are removed.

@inline(never)
func inCycleA() {
	inCycleB()
}

@inline(never)
func inCycleB() {
	inCycleA()
}

// Check if unused vtable methods are removed.

class Base {

	@inline(never)
	func aliveMethod() {
	}

	@inline(never)
	func deadMethod() {
		// introduces a cycle
		testClasses(self)
	}

	// alive, because called with super
	@inline(never)
	func calledWithSuper() {
	}

	// dead, because only overridden method is called
	@inline(never)
	func baseNotCalled() {
	}

	// alive, because called for Derived but not overridden in Derived
	@inline(never)
	func notInDerived() {
	}

	// dead, because only overridden method is called
	@inline(never)
	func notInOther() {
	}
}

class Derived : Base {

	@inline(never)
	override func aliveMethod() {
	}

	@inline(never)
	override func deadMethod() {
	}

	@inline(never)
	@_semantics("optimize.sil.never") // avoid devirtualization
	override func calledWithSuper() {
		super.calledWithSuper()
	}

	@inline(never)
	override func baseNotCalled() {
	}

	@inline(never)
	override func notInOther() {
	}
}

class Other : Derived {
	@inline(never)
	override func baseNotCalled() {
	}

	@inline(never)
	override func notInDerived() {
	}
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testClasses(_ b: Base) {
	b.aliveMethod()
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testWithDerived(_ d: Derived) {
	d.baseNotCalled()
	d.notInDerived()
	d.calledWithSuper()
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testWithOther(_ o: Other) {
	o.notInOther()
}

// Check if dead methods of classes with higher visibility are removed.

public class PublicClass {
	func publicClassMethod() {
	}
}

// Check if unused witness table methods are removed.

protocol Prot {
	func aliveWitness()

	func deadWitness()
}

struct Adopt : Prot {
	@inline(never)
	func aliveWitness() {
	}

	@inline(never)
	func deadWitness() {
	}
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testProtocols(_ p: Prot) {
	p.aliveWitness()
}


@_semantics("optimize.sil.never") // avoid devirtualization
public func callTest() {
	testClasses(Base())
	testClasses(Derived())
	testWithDerived(Derived())
	testWithOther(Other())
	testProtocols(Adopt())
}

@_semantics("optimize.sil.never") // make sure not eliminated 
internal func donotEliminate() {
  return
}

// CHECK-NOT: sil {{.*}}inCycleA
// CHECK-NOT: sil {{.*}}inCycleB
// CHECK-NOT: sil {{.*}}deadMethod
// CHECK-NOT: sil {{.*}}deadWitness
// CHECK-NOT: sil {{.*}}publicClassMethod

// CHECK-TESTING: sil {{.*}}inCycleA
// CHECK-TESTING: sil {{.*}}inCycleB
// CHECK-TESTING: sil {{.*}}deadMethod
// CHECK-TESTING: sil {{.*}}publicClassMethod
// CHECK-TESTING: sil {{.*}}deadWitness

// CHECK-LABEL: @_TF25dead_function_elimination14donotEliminateFT_T_

// CHECK-LABEL: sil_vtable Base
// CHECK: aliveMethod
// CHECK: calledWithSuper
// CHECK-NOT: deadMethod
// CHECK-NOT: baseNotCalled
// CHECK: notInDerived
// CHECK-NOT: notInOther

// CHECK-TESTING-LABEL: sil_vtable Base
// CHECK-TESTING: deadMethod

// CHECK-LABEL: sil_vtable Derived
// CHECK: aliveMethod
// CHECK-NOT: deadMethod
// CHECK: baseNotCalled
// CHECK: notInDerived
// CHECK: notInOther

// CHECK-TESTING-LABEL: sil_vtable Derived
// CHECK-TESTING: deadMethod

// CHECK-LABEL: sil_vtable Other
// CHECK: aliveMethod
// CHECK-NOT: deadMethod
// CHECK: baseNotCalled
// CHECK: notInDerived
// CHECK: notInOther

// CHECK-LABEL: sil_witness_table hidden Adopt: Prot
// CHECK: aliveWitness!1: @{{.*}}aliveWitness
// CHECK: deadWitness!1: nil

// CHECK-TESTING-LABEL: sil_witness_table [fragile] Adopt: Prot
// CHECK-TESTING: deadWitness{{.*}}: @{{.*}}deadWitness

