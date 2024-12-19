// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-build-swift -O %s -emit-sil |  %FileCheck %s
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefix=CHECK-OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib,swift_stdlib_no_asserts

var a = [27]

var gg = ""

@inline(never)
func use(_ s: String) {
  gg = s
}

// CHECK-LABEL: sil hidden [noinline] @$s25array_element_propagation10testAppendyySaySiGzF :
// CHECK-NOT:     load
// CHECK:       } // end sil function '$s25array_element_propagation10testAppendyySaySiGzF'
@inline(never)
func testAppend(_ arr: inout [Int]) {
  let a = [28]
  arr += [a[0]]
}

// CHECK-LABEL: sil hidden [noinline] @$s25array_element_propagation8testLoopyyF :
// CHECK-NOT:     load
// CHECK:       } // end sil function '$s25array_element_propagation8testLoopyyF'
@inline(never)
func testLoop() {
  let a = ["xyz"]

  for _ in 0..<1000 {
    use(a[0])
  }
}

// CHECK-LABEL: sil hidden [noinline] @$s25array_element_propagation12testNonConstyySSF :
// CHECK-NOT:     load
// CHECK:       } // end sil function '$s25array_element_propagation12testNonConstyySSF'
@inline(never)
func testNonConst(_ s: String) {
  let a = [s]

  for _ in 0..<1000 {
    use(a[0])
  }
}


// CHECK-OUTPUT: [27, 28]
testAppend(&a)
print(a)

// CHECK-OUTPUT: xyz
testLoop()
print(gg)

// CHECK-OUTPUT: abc
testNonConst("abc")
print(gg)
