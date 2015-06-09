// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | FileCheck %s
// REQUIRES: asserts,swift_stdlib_no_asserts,optimized_stdlib

// CHECK-LABEL: COW Array Opts in Func {{.*}}inoutarr{{.*}}
// CHECK: Hoisting make_mutable
// CHECK: COW Array Opts
func inoutarr(inout a: [Int]) {
  for i in 0..<a.count {
    a[i] = 0
  }
}

struct S {
  var a: [Int]
}

// CHECK-LABEL: COW Array Opts in Func {{.*}}arrelt{{.*}}
// CHECK: Hoisting make_mutable
// CHECK: COW Array Opts
func arrelt(inout s: S) {
  for i in 0..<s.a.count {
    s.a[i] = 0
  }
}

class ArrayInClass {
  final var A : [Int]
  final var B : [Int]
  final var C : [[Int]]

  init() {
    A = []
    B = []
    C = [[]]
  }

  // CHECK-LABEL: COW Array Opts in Func {{.*}}hoistInClass{{.*}}
  // CHECK: Hoisting make_mutable
  func hoistInClass() {
    for i in 0..<A.count {
      A[i] = 0
    }
  }

  // CHECK-LABEL: COW Array Opts in Func {{.*}}hoistInClass2Arr{{.*}}
  // CHECK: Hoisting make_mutable
  func hoistInClass2Arr() {
    for i in 0..<A.count {
      A[i] = 0
      B[i] = 2
    }
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

struct Array2d {
  var A : [Int]
  var cols : Int
  var rows : Int
}

// CHECK-LABEL: COW Array Opts in Func {{.*}}test2DArrayLoop{{.*}}
// CHECK:        Array Opts in Loop Loop at depth 2
// CHECK-NOT:   COW Array Opts in
// CHECK:        Hoisting make_mutable
// CHECK:        Array Opts in Loop Loop at depth 1
// CHECK-NOT:   COW Array Opts in
// CHECK:        Hoisting make_mutable

func test2DArrayLoop(inout A : Array2d) {
  for r in 0 ..< A.rows {
    for c in 0 ..< A.cols {
      A.A[r*A.cols+c] += 1
    }
  }
}

class AClass {}

// CHECK-LABEL: COW Array Opts in Func {{.*}}hoistArrayOfClasses{{.*}}
// CHECK: Hoisting make_mutable
// CHECK: COW Array Opts

func hoistArrayOfClasses(inout A : [AClass], x: AClass) {
  for i in 0 ..< A.count {
    A[i] = x
  }
}
