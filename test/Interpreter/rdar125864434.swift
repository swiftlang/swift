// RUN: %empty-directory(%t)
// RUN: %target-build-swift  %s -o %t/bin
// RUN: %target-codesign %t/bin
// RUN: %target-run %t/bin | %FileCheck %s

// REQUIRES: executable_test

protocol Boopable: ~Copyable {
  func boop()
}

struct S: ~Copyable, Boopable {
  func boop() { print("boop") }
}

func check(_ b: borrowing any Boopable & ~Copyable) {
  b.boop()
}

// CHECK: boop
check(S())
