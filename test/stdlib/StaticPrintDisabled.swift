// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s                                -I %t -o %t/a1.out -O && %target-run %t/a1.out | %FileCheck %s
// RUN: %target-build-swift %s -D SWIFT_STDLIB_PRINT_DISABLED -I %t -o %t/a2.out -O && %target-run %t/a2.out | %FileCheck %s --check-prefix CHECK-PRINT-DISABLED --allow-empty

// REQUIRES: executable_test
// REQUIRES: stdlib_static_print

let x = 42
let s = "ABCDE"
print("Hello World \(5) \(x) \(s)")

// CHECK: Hello World 5 42 ABCDE
// CHECK-PRINT-DISABLED-NOT: Hello World
