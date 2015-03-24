// RUN: %target-swift-frontend %s -O -emit-sil | FileCheck %s
// RUN: %target-swift-frontend %s -O -emit-sil -enable-testing | FileCheck -check-prefix=CHECK-TESTING %s

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
}

class Derived : Base {

	@inline(never)
	override func aliveMethod() {
	}

	@inline(never)
	override func deadMethod() {
	}
}

@inline(never)
func testClasses(b: Base) {
	b.aliveMethod()
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
func testProtocols(p: Prot) {
	p.aliveWitness()
}


public func callTest() {
	testClasses(Base())
	testClasses(Derived())
	testProtocols(Adopt())
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

// CHECK-LABEL: sil_vtable Base
// CHECK-NOT: deadMethod

// CHECK-TESTING-LABEL: sil_vtable Base
// CHECK-TESTING: deadMethod

// CHECK-LABEL: sil_vtable Derived
// CHECK-NOT: deadMethod

// CHECK-TESTING-LABEL: sil_vtable Derived
// CHECK-TESTING: deadMethod

// CHECK-LABEL: sil_witness_table hidden Adopt: Prot
// CHECK: deadWitness{{.*}} nil

// CHECK-TESTING-LABEL: sil_witness_table Adopt: Prot
// CHECK-TESTING: deadWitness{{.*}}: @{{.*}}deadWitness



// <rdar://problem/19267795> failable initializers that call noreturn function produces bogus diagnostics
class FailableInitThatFailsReallyHard {
  init?() {   // no diagnostics generated.
    fatalError("bad")
  }
}


class BaseClass {}
final class DerivedClass : BaseClass {
  init(x : ()) {
    fatalError("bad")  // no diagnostics.
  }
}




