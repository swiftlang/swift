// Regression test: -warn-long-expression-type-checking-scopes must emit a
// warning when an expression uses more solver scopes than the threshold.
// Using threshold=1 guarantees the warning fires on any non-trivial expression,
// making the test deterministic across machines and build configurations.
//
// RUN: %target-swiftc_driver -Xfrontend -warn-long-expression-type-checking-scopes=1 %s 2>&1 | %FileCheck %s
// CHECK: expression took {{[0-9]+}} scope(s) to type-check (limit: 1)

func example() {
    let _: Int = [1, 2, 3].map { $0 * 2 }.reduce(0, +)
}
