// RUN: %target-swift-emit-silgen %s | %FileCheck %s

struct Foo: ~Copyable {
    var bar: Bar<Baz>
}

struct Bar<T: ~Copyable>: ~Copyable {
    var elements: T

    subscript(i: Int) -> T {
        unsafeAddress {
            fatalError()
        }
    }
}

struct Baz: ~Copyable {
    var storage: Int

    borrowing func test() {}
}

// CHECK-LABEL: sil {{.*}} @${{.*}}4test1x
func test(x: borrowing Foo) {
    // The materialization of x.bar into a temporary to invoke the subscript
    // addressor needs to cover the entire formal evaluation scope of the 
    // projection, since the pointer may depend on the materialized
    // representation

    // CHECK: [[BAR_MAT:%.*]] = alloc_stack $Bar<Baz>
    // CHECK: [[BAR_MAT_BORROW:%.*]] = store_borrow {{%.*}} to [[BAR_MAT]]
    // CHECK: [[TEST_FN:%.*]] = function_ref @${{.*}}3BazV4test
    // CHECK: apply [[TEST_FN]]
    // CHECK: end_borrow [[BAR_MAT_BORROW]]
    // CHECK: dealloc_stack [[BAR_MAT]]
    x.bar[0].test()
}

// Value borrows should also cover the scope of the projected value, at
// least until it's copied out if copyable.

struct CopyableLoadable {
    var x: AnyObject

    subscript(i: Int) -> CopyableResult {
        unsafeAddress {
            fatalError()
        }
    }
}

struct CopyableResult {
    var x: AnyObject

    func test() {}
}

func copyableLoadable() -> CopyableLoadable { fatalError() }

// CHECK-LABEL: sil {{.*}} @${{.*}}5test2
func test2() {

    // CHECK: [[BASE:%.*]] = begin_borrow
    // CHECK: [[ADDRESSOR:%.*]] = function_ref @${{.*}}16CopyableLoadableVy{{.*}}
    // CHECK: [[PTR:%.*]] = apply [[ADDRESSOR]]({{.*}}, [[BASE]])
    // CHECK: [[RAWPTR:%.*]] = struct_extract [[PTR]]
    // CHECK: [[ADDR:%.*]] = pointer_to_address [[RAWPTR]]
    // CHECK: [[DEP_ADDR:%.*]] = mark_dependence [unresolved] [[ADDR]] on [[BASE]]
    // CHECK: [[DEP_ADDR_ACCESS:%.*]] = begin_access [read] [unsafe] [[DEP_ADDR]]
    // CHECK: load [copy] [[DEP_ADDR_ACCESS]]
    // CHECK: end_access [[DEP_ADDR_ACCESS]]
    // CHECK: end_borrow [[BASE]]
    copyableLoadable()[0].test()
}
