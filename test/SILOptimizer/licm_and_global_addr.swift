// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %S/Inputs/licm_and_global_addr/test.swift -parse-as-library -wmo -enable-library-evolution -module-name=Test -emit-module -emit-module-path=%t/Test.swiftmodule -c -o %t/test.o
// RUN: %target-build-swift -O %S/Inputs/licm_and_global_addr/main.swift %s -I%t %t/test.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

import Test

// Check that LICM does not move a global_addr above an alloc_global in case
// the global might be dynamically allocated.

// The type Abc is resilient and > 3 words. So it is allocated dynamically in a buffer.
var a = Abc()

@inline(never)
func testit(_ n: Int) {
  for _ in 0..<n {
    // Prevent hoisting the initialization of 'a', 
    // which would defeat the purpose of the test.
    unknown()

// CHECK: 1
// CHECK: 1
// CHECK: 1
    a.printit()
  }
}

