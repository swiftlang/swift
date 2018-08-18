// RUN: %target-build-swift -Onone -emit-silgen %s -o %t.sil
// RUN: %target-build-swift -parse-sil %t.sil -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t | %FileCheck %s

// RUN: %target-build-swift -Onone -c %t.sil -o %t.o
// RUN: %target-build-swift %t.o -o %t-2
// RUN: %target-codesign %t-2
// RUN: %target-run %t-2 | %FileCheck %s
// REQUIRES: executable_test

// CHECK: Hello World
// CHECK: Hello Bob, today is Tuesday.

@inlinable
@usableFromInline
func greet(_ name: String, _ day: String) -> String {
  return "Hello \(name), today is \(day)."
}

print("Hello World")
print(greet("Bob", "Tuesday"))
