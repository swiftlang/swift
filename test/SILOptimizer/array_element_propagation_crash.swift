// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Check that ArrayElementPropagation does not crash when compiling this function.
@inline(never)
func testit(_ arr: inout [Int]) {
  let a = [28]
  arr += [a[0]]
}

var a = [27]
testit(&a)

// As a bonus, also check if the code works as expected.
// CHECK: [27, 28]
print(a)

