// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -import-objc-header %S/Inputs/dse_with_union.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

// Check that EscapeAnalysis does not mess up with a union.

@inline(never)
func printit(_ u: inout U) {
  print(u.p![0])
}

@inline(never)
func testit(s: S) {
  var vs = s
  withUnsafeMutablePointer(to: &vs) {
    // A C union is an opaque type in swift and not considered as a "pointer"
    // in escape analysis. Converting between pointer and non-pointer resulted
    // in a wrong connection graph.
    var u = U(p: $0)
    printit(&u)
  }
}

// CHECK: S(i: 27)
testit(s: S(i: 27))

