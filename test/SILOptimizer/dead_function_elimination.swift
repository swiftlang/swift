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
	func DeadMethod() {
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
	override func DeadMethod() {
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

public class PublicCl {
	func publicClassMethod() {
	}
}

// Check if unused witness table methods are removed.

protocol Prot {
  func aliveWitness()

  func DeadWitness()

  func aliveDefaultWitness()

  func DeadDefaultWitness()
}

extension Prot {
  @inline(never)
  func aliveDefaultWitness() {
  }

  @inline(never)
  func DeadDefaultWitness() {
  }
}

struct Adopt : Prot {
	@inline(never)
	func aliveWitness() {
	}

	@inline(never)
	func DeadWitness() {
	}
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testProtocols(_ p: Prot) {
	p.aliveWitness()
}

@inline(never)
@_semantics("optimize.sil.never") // avoid devirtualization
func testDefaultWitnessMethods(_ p: Prot) {
	p.aliveDefaultWitness()
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
// CHECK-NOT: sil {{.*}}DeadMethod
// CHECK-NOT: sil {{.*}}DeadWitness
// CHECK-NOT: sil {{.*}}publicClassMethod

// CHECK-TESTING: sil {{.*}}inCycleA
// CHECK-TESTING: sil {{.*}}inCycleB
// CHECK-TESTING: sil {{.*}}DeadMethod
// CHECK-TESTING: sil {{.*}}publicClassMethod
// CHECK-TESTING: sil {{.*}}DeadWitness

// CHECK-LABEL: @_T025dead_function_elimination14donotEliminateyyF

// CHECK-LABEL: sil_vtable Base
// CHECK: aliveMethod
// CHECK: calledWithSuper
// CHECK-NOT: DeadMethod
// CHECK-NOT: baseNotCalled
// CHECK: notInDerived
// CHECK-NOT: notInOther

// CHECK-TESTING-LABEL: sil_vtable Base
// CHECK-TESTING: DeadMethod

// CHECK-LABEL: sil_vtable Derived
// CHECK: aliveMethod
// CHECK-NOT: DeadMethod
// CHECK: baseNotCalled
// CHECK: notInDerived
// CHECK: notInOther

// CHECK-TESTING-LABEL: sil_vtable Derived
// CHECK-TESTING: DeadMethod

// CHECK-LABEL: sil_vtable Other
// CHECK: aliveMethod
// CHECK-NOT: DeadMethod
// CHECK: baseNotCalled
// CHECK: notInDerived
// CHECK: notInOther

// CHECK-LABEL: sil_witness_table hidden Adopt: Prot
// CHECK: aliveWitness!1: {{.*}} : @{{.*}}aliveWitness
// CHECK: DeadWitness!1: {{.*}} : nil

// CHECK-TESTING-LABEL: sil_witness_table [serialized] Adopt: Prot
// CHECK-TESTING: DeadWitness{{.*}}: @{{.*}}DeadWitness

// CHECK-LABEL: sil_default_witness_table hidden Prot
// CHECK:  no_default
// CHECK:  no_default
// CHECK:  method #Prot.aliveDefaultWitness!1: {{.*}} : @{{.*}}aliveDefaultWitness
// CHECK:  no_default

// CHECK-TESTING-LABEL: sil_default_witness_table Prot
// CHECK-TESTING:  no_default
// CHECK-TESTING:  no_default
// CHECK-TESTING:  method #Prot.aliveDefaultWitness!1: {{.*}} : @{{.*}}aliveDefaultWitness
// CHECK-TESTING:  method #Prot.DeadDefaultWitness!1: {{.*}} : @{{.*}}DeadDefaultWitness
