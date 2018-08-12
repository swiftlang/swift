// RUN: %target-build-swift -Onone -emit-silgen %s -o %t.sil
// RUN: %target-build-swift -parse-sil %t.sil -o %t
// RUN: %target-run %t | %FileCheck %s

// RUN: %target-build-swift -Onone -c %t.sil -o %t.o
// RUN: %target-build-swift %t.o -o %t
// RUN: %target-run %t | %FileCheck %s
// REQUIRES: executable_test

// CHECK: Hello World
// CHECK: Hello Bob, today is Tuesday.

// This test intentionally mirrors /Driver/emit-sib-single-file to ensure that
// SwiftOnoneSupport is always a dependency of -Onone -emit-si*gen builds.

// FIXME: The Frontend's understanding of the situations in which to load
// SwiftOnoneSupport is a tacit part of the rest of the compile pipeline and
// pervades the AST.  SIL could probably sink knowledge of module dependencies
// internally and make this test unnecessary.

@inlinable
@usableFromInline
func greet(_ name: String, _ day: String) -> String {
  return "Hello \(name), today is \(day)."
}

print("Hello World")
print(greet("Bob", "Tuesday"))
