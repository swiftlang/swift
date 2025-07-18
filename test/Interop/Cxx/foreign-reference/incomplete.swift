// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/incomplete.c -I %S/Inputs -o %t/incomplete-c.o

// Build with C interoperatibility
// RUN: %target-build-swift %s -I %S/Inputs -o %t/incomplete %t/incomplete-c.o
// RUN: %target-codesign %t/incomplete
// RUN: %target-run %t/incomplete | %FileCheck %s

// Build with C++ interoperability
// RUN: %target-build-swift %s -I %S/Inputs -o %t/incomplete %t/incomplete-c.o -Xfrontend -cxx-interoperability-mode=default
// RUN: %target-codesign %t/incomplete
// RUN: %target-run %t/incomplete | %FileCheck %s

// REQUIRES: executable_test

import ReferenceCounted

func testIncomplete() {
  do {
    let i = Incomplete(weight: 3.14159)
    // CHECK: Incomplete weight = 3.14159
    print("Incomplete weight = \(i.weight)")

    // Instance destroyed at the end
  }

  // CHECK: Destroyed instance containing weight 3.14159
}

testIncomplete()
