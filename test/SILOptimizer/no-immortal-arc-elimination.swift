// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts -module-name=test -O -target arm64-apple-ios12.5.8 -emit-sil | %FileCheck %s

// REQUIRES: OS=ios
// REQUIRES: CPU=arm64

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// Check that the optimizer does not remove "unbalanced" retains for immortal objects
// prior to a Swift 5.1 runtime (which does not support immortal objects).

// CHECK-LABEL: sil hidden [noinline] @$s4test10emptyArraySaySiGyF
// CHECK:   global_addr
// CHECK:   retain
// CHECK: } // end sil function '$s4test10emptyArraySaySiGyF'
@inline(never)
func emptyArray() -> [Int] {
  let x = [Int]()
  return x
}

// CHECK-LABEL: sil hidden [noinline] @$s4test13constantArraySaySiGyF
// CHECK-NOT:     global_value
// CHECK:         alloc_ref
// CHECK-NOT:     global_value
// CHECK:       } // end sil function '$s4test13constantArraySaySiGyF'
@inline(never)
func constantArray() -> [Int] {
  return [1, 2, 3]
}

