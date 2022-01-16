// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts -module-name=test -O -target %target-cpu-apple-macos10.15 -emit-sil | %FileCheck %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -Xllvm -sil-disable-pass=function-signature-opts -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: OS=macosx
// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler

// Check that the optimizer can remove "unbalanced" retains for immortal objects.
// But only with a Swift 5.1 runtime (which supports immortal objects).

// CHECK-LABEL: sil hidden [noinline] @$s4test10emptyArraySaySiGyF
// CHECK:       global_addr
// CHECK-NOT:   retain
// CHECK: } // end sil function '$s4test10emptyArraySaySiGyF'
@inline(never)
func emptyArray() -> [Int] {
  let x = [Int]()
  return x
}

// CHECK-LABEL: sil hidden [noinline] @$s4test13constantArraySaySiGyF
// CHECK:       global_value
// CHECK-NOT:   retain
// CHECK: } // end sil function '$s4test13constantArraySaySiGyF'
@inline(never)
func constantArray() -> [Int] {
  return [1, 2, 3]
}

func testit() {
  // CHECK-OUTPUT: []
  // CHECK-OUTPUT: [1, 2, 3]
  // CHECK-OUTPUT: []
  // CHECK-OUTPUT: [1, 2, 3]
  // CHECK-OUTPUT: []
  // CHECK-OUTPUT: [1, 2, 3]
  for _ in 0..<3 {
    print(emptyArray())
    print(constantArray())
  }
}

testit()


