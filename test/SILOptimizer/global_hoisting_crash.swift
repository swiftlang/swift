// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

struct Teststruct {
  static let s = Teststruct()

  @inline(never)
  init() {
    let set = Set<String>()
    for _ in set {
      // Check that the global initializer is not hoisted out of this loop,
      // resulting in a dispatch_once re-retrance crash.
      _ = Teststruct.s
    }
  }
}

// CHECK: Teststruct
print(Teststruct.s)


