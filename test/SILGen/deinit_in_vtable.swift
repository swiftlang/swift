// RUN: %target-swift-emit-sil -Xllvm -sil-full-demangle -O %s | %FileCheck %s

// The second run tests is it can be compiled without crashes.
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -O -S %s

private class A {
	func foo() -> Int { return 0 }
}

// CHECK-LABEL: deinit_in_vtable.(A in {{.*}}).__deallocating_deinit
// CHECK: sil private @[[A:.*]] :

private class B : A {
	override func foo() -> Int { return 1 }
}

// CHECK-LABEL: deinit_in_vtable.(B in {{.*}}).__deallocating_deinit
// CHECK: sil private @[[B:.*]] :

@inline(never)
private func testfunc(_ a: A) -> Int {
  return a.foo()
}

public func testmain() {
	testfunc(B())
}

// Check if the deallocating destructors are listed in the vtable.
// This is required so that the are not removed (if not public) by dead
// function elimination

// CHECK-LABEL: sil_vtable A
// CHECK: A.deinit!deallocator.1: @[[A]]

// CHECK-LABEL: sil_vtable B
// CHECK-NOT: A.deinit
// CHECK: B.deinit!deallocator.1: @[[B]]

