// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST1
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST2
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST3
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST4
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST5
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST6
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST7
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST8
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST9
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST10
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST11
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST12
// RUN: %target-swift-frontend -O -emit-sil -Xllvm -debug-only=cowarray-opts -primary-file %s 2>&1 | %FileCheck %s --check-prefix=TEST13
// REQUIRES: asserts,swift_stdlib_no_asserts,optimized_stdlib

// TEST1-LABEL: COW Array Opts in Func {{.*}}inoutarr{{.*}}
// TEST1: Hoisting make_mutable
// TEST1: COW Array Opts

func inoutarr(a: inout [Int]) {
  for i in 0..<a.count {
    a[i] = 0
  }
}

struct S {
  var a: [Int]
}

// TEST2-LABEL: COW Array Opts in Func {{.*}}arrelt{{.*}}
// TEST2: Hoisting make_mutable
// TEST2: COW Array Opts
func arrelt(s: inout S) {
  for i in 0..<s.a.count {
    s.a[i] = 0
  }
}


class Array_in_class {
  final var A : [Int]
  final var B : [Int]
  final var C : [[Int]]

  init() {
    A = []
    B = []
    C = [[]]
  }

  // TEST3-LABEL: COW Array Opts in Func {{.*}}hoistInClass{{.*}}
  // TEST3: Hoisting make_mutable
  // TEST3: COW Array Opts
  func hoistInClass() {
    for i in 0..<A.count {
      A[i] = 0
    }
  }

  // TEST4-LABEL: COW Array Opts in Func {{.*}}hoistInClass2Arr{{.*}}
  // TEST4: Hoisting make_mutable
  // TEST4: COW Array Opts
  func hoistInClass2Arr() {
    for i in 0..<A.count {
      A[i] = 0
      B[i] = 2
    }
  }

  // TEST5-LABEL: COW Array Opts in Func {{.*}}dontHoistInClassAppend{{.*}}
  // TEST5-NOT: Hoisting make_mutable
  // TEST5: COW Array Opts in Func
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

// TEST6-LABEL: COW Array Opts in Func {{.*}}test2DArrayLoop{{.*}}
// TEST6:        Array Opts in Loop Loop at depth 2
// TEST6-NOT:   COW Array Opts in
// TEST6:        Hoisting make_mutable
// TEST6:        Array Opts in Loop Loop at depth 1
// TEST6-NOT:   COW Array Opts in
// TEST6:        Hoisting make_mutable
// TEST6:   COW Array Opts in

func test2DArrayLoop(A: inout Array2d) {
  for r in 0 ..< A.rows {
    for c in 0 ..< A.cols {
      A.A[r*A.cols+c] += 1
    }
  }
}

class AClass {}

// TEST7-LABEL: COW Array Opts in Func {{.*}}hoistArrayOfClasses{{.*}}
// TEST7: Hoisting make_mutable
// TEST7: COW Array Opts
func hoistArrayOfClasses(A: inout [AClass], x: AClass) {
  for i in 0 ..< A.count {
    A[i] = x
  }
}

// TEST8-LABEL: COW Array Opts in Func {{.*}}hoistMutableOfAppend{{.*}}
// TEST8: Hoisting make_mutable
// TEST8: COW Array Opts

func hoistMutableOfAppend(A: inout [Int]) {
  for i in 0 ..< 10 {
    A.append(i)
  }
}

@inline(never)
func use(_ a: [Int]) {
}

class ArrayHolder {
  final var A : [[Int]]
  init() { A = [] }

// TEST9-LABEL: COW Array Opts in Func {{.*}}hoist2DArrayInClass
// TEST9: Hoisting make_mutable
// TEST9: COW Array Opts

  func hoist2DArrayInClass() {
    for i in 0 ..< 10 {
      for y in 0 ..< 10 {
        A[i][y] = A[i][y] * 2
      }
    }
  }

// TEST10-LABEL: COW Array Opts in Func {{.*}}dontHoist2DArray
// TEST10-NOT: Hoisting make_mutable
// TEST10: COW Array Opts

  func dontHoist2DArray(){
    var escape : [Int] = []
    for i in 0 ..< A.count {
      for y in 0 ..< A[i].count {
        escape = A[i]
        A[i][y] = A[i][y] * 2
      }
    }
    use(escape)
  }

// TEST11-LABEL: COW Array Opts in Func {{.*}}dontHoist2DArray2
// TEST11-NOT: Hoisting make_mutable
// TEST11: COW Array Opts

  func dontHoist2DArray2(){
    let b = [0]
    for i in 0 ..< A.count {
      for y in 0 ..< A[i].count {
        A[1] = b
        A[i][y] = A[i][y] * 2
      }
    }
  }

// TEST12-LABEL: COW Array Opts in Func {{.*}}dontHoist2DArray3
// TEST12-NOT: Hoisting make_mutable
// TEST12: COW Array Opts

  func dontHoist2DArray3() {
    for i in 0 ..< A.count {
      for y in 0 ..< A[i].count {
        A[i].append(2)
        A[i][y] = A[i][y] * 2
      }
    }
  }
}

// TEST13-LABEL: COW Array Opts in Func {{.*}}hoist2DArray
// TEST13: Hoisting make_mutable
// TEST13: COW Array Opts

func hoist2DArray(a: inout [[Int]]) {
  for i in 0 ..< a.count {
    for y in 0 ..< a[i].count {
      a[i][y] = a[i][y] * 2
    }
  }
}
