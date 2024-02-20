// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -target %target-future-triple -O %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test,swift_stdlib_no_asserts,optimized_stdlib
// REQUIRES: objc_interop
// REQUIRES: macos_min_version_13

// UNSUPPORTED: use_os_stdlib

import Foundation

@inline(never)
func createArray() -> [Int] {
  return [1, 2, 3]
}

@inline(never)
func printNSArray(_ a: NSArray) {
  // CHECK: 1
  print(a[0])
  // CHECK: 2
  print(a[1])
  // CHECK: 3
  print(a[2])
}


@inline(never)
func testit() {
  let a = createArray()
  printNSArray(a as NSArray)
}

testit()
