// REQUIRES: updating_for_owned_nonescaping_closures

// RUN: %target-build-swift -Onone -emit-sib %s -o %t.sib
// RUN: %target-build-swift %t.sib -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t | %FileCheck %s

// RUN: %target-build-swift -Onone -c %t.sib -o %t.o
// RUN: %target-build-swift %t.o -o %t-2
// RUN: %target-codesign %t-2
// RUN: %target-run %t-2 | %FileCheck %s

// RUN: %target-build-swift -Onone -emit-sibgen %s -o %t.sib
// RUN: %target-build-swift %t.sib -o %t-3
// RUN: %target-codesign %t-3
// RUN: %target-run %t-3 | %FileCheck %s

// RUN: %target-build-swift -Onone -c %t.sib -o %t.o
// RUN: %target-build-swift %t.o -o %t-4
// RUN: %target-codesign %t-4
// RUN: %target-run %t-4 | %FileCheck %s
// REQUIRES: executable_test

// CHECK: Hello World
// CHECK: Hello Bob, today is Tuesday.

// This test intentionally mirrors /Driver/emit-sil-single-file to ensure that
// SwiftOnoneSupport is always a dependency of -Onone -emit-si*gen builds.

@inlinable
@usableFromInline
func greet(_ name: String, _ day: String) -> String {
  return "Hello \(name), today is \(day)."
}

print("Hello World")
print(greet("Bob", "Tuesday"))
