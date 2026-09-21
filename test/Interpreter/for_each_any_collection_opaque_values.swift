// RUN: %target-run-simple-swift(-Xfrontend -enable-sil-opaque-values) | %FileCheck %s
// REQUIRES: executable_test

// Execution test for `for v in c` over `any Collection` under opaque values.
// The phi storage for the loop's per-iteration `Optional<Element>` is
// allocated outside the loop; per-iteration `@out` slots are alloc'd/dealloc'd
// each iteration. This test verifies the loop iterates exactly the right
// number of times and produces the right values at runtime.

func testForEachAnyCollection(_ c: any Collection) {
  for v in c {
    print(v)
  }
}

print("--- Int ---")
testForEachAnyCollection([1, 2, 3])
print("--- Mixed ---")
testForEachAnyCollection([1, 2.0, "Hello"] as [Any])
print("--- Empty ---")
testForEachAnyCollection([] as [Int])
print("--- Done ---")

// CHECK:      --- Int ---
// CHECK-NEXT: 1
// CHECK-NEXT: 2
// CHECK-NEXT: 3
// CHECK-NEXT: --- Mixed ---
// CHECK-NEXT: 1
// CHECK-NEXT: 2.0
// CHECK-NEXT: Hello
// CHECK-NEXT: --- Empty ---
// CHECK-NEXT: --- Done ---
