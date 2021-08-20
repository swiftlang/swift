// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts -module-name=test -O -target %target-cpu-apple-macos10.14 -emit-sil | %FileCheck --check-prefix=CHECK-SWIFT4x %s
// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts -module-name=test -O -target %target-cpu-apple-macos10.15 -emit-sil | %FileCheck --check-prefix=CHECK-SWIFT50 %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -Xllvm -sil-disable-pass=function-signature-opts -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: OS=macosx
// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: libswift

// Check that the optimizer can remove "unbalanced" retains for immortal objects.
// But only with a Swift 5.1 runtime (which supports immortal objects).

// CHECK-SWIFT4x-LABEL: sil hidden [noinline] @$s4test10emptyArraySaySiGyF
// CHECK-SWIFT4x:   global_addr
// CHECK-SWIFT4x:   retain
// CHECK-SWIFT4x: } // end sil function '$s4test10emptyArraySaySiGyF'

// CHECK-SWIFT50-LABEL: sil hidden [noinline] @$s4test10emptyArraySaySiGyF
// CHECK-SWIFT50:       global_addr
// CHECK-SWIFT50-NOT:   retain
// CHECK-SWIFT50: } // end sil function '$s4test10emptyArraySaySiGyF'
@inline(never)
func emptyArray() -> [Int] {
  let x = [Int]()
  return x
}

// CHECK-SWIFT4x-LABEL: sil hidden [noinline] @$s4test13constantArraySaySiGyF
// CHECK-SWIFT4x:   global_value
// CHECK-SWIFT4x:   retain
// CHECK-SWIFT4x: } // end sil function '$s4test13constantArraySaySiGyF'

// CHECK-SWIFT50-LABEL: sil hidden [noinline] @$s4test13constantArraySaySiGyF
// CHECK-SWIFT50:       global_value
// CHECK-SWIFT50-NOT:   retain
// CHECK-SWIFT50: } // end sil function '$s4test13constantArraySaySiGyF'
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


