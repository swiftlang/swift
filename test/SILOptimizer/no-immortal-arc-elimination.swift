// RUN: %target-swift-frontend -primary-file %s -O -sil-verify-all -Xllvm -sil-disable-pass=function-signature-opts -module-name=test -O -target %target-cpu-apple-macos10.14 -emit-sil | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: CPU=x86_64
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: swift_in_compiler

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
// CHECK:   global_value
// CHECK:   retain
// CHECK: } // end sil function '$s4test13constantArraySaySiGyF'
@inline(never)
func constantArray() -> [Int] {
  return [1, 2, 3]
}

