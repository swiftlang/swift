// RUN: %target-swift-frontend  -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check all components, including IRGen.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefix=CHECK-OUTPUT %s

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib

protocol P {
    func foo() throws -> Int
}

struct S: P {
    var a: Int
    var b: Int
    var c: Int
    var d: Int

    func foo() throws -> Int {
        return a + b + c + d
    }
}

func callClosure<R>(_ body: () throws -> R) rethrows -> R {
    try body()
}

// Check that the optimizer can eliminat all calls to thunks and directly
// try_apply's the witness method.

// CHECK-LABEL: sil hidden [noinline] @$s4test6testitySiSgAA1P_pF
// CHECK:   [[METHOD:%[0-9]+]] = witness_method $@opened{{.*}} #P.foo
// CHECK:   try_apply [[METHOD]]<@opened
// CHECK: // end sil function '$s4test6testitySiSgAA1P_pF'
@inline(never)
func testit(_ p: P) -> Int? {
    return try? callClosure(p.foo)
}

let p: P = S(a: 1, b: 2, c: 3, d: 4)

// CHECK-OUTPUT: 10
print(testit(p)!)

