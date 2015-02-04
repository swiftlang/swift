// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | FileCheck %s
// REQUIRES: asserts

class ArrayInClass {
  final var A : [Int]
  final var B : [Int]
  final var C : [[Int]]

  init() {
    A = []
    B = []
    C = [[]]
  }


  // CHECK-LABEL: COW Array Opts in Func {{.*}}dontHoistInClassAppend{{.*}}
  // CHECK-NOT: Hoisting make_mutable
  // CHECK: COW Array Opts in Func
  func dontHoistInClassAppend() {
    for i in 0..<A.count {
      A[i] = 0
      C.append(A)
    }
  }
}
