func intTaker(_ a: Int) {}

func foo() {
  _ = 1234567
  _ = 1234567.12345
  intTaker(123456)
  _ = 1234567 + 1234567
  _ = 1234567.12345 + 1234567.12345

  _ = 123
  _ = 123.123
}

// RUN: %refactor -source-filename %s -pos=4:10 | %FileCheck %s -check-prefix=CHECK-LONG-NUMER
// RUN: %refactor -source-filename %s -pos=5:10 | %FileCheck %s -check-prefix=CHECK-LONG-NUMER
// RUN: %refactor -source-filename %s -pos=6:15 | %FileCheck %s -check-prefix=CHECK-LONG-NUMER
// RUN: %refactor -source-filename %s -pos=7:10 | %FileCheck %s -check-prefix=CHECK-LONG-NUMER
// RUN: %refactor -source-filename %s -pos=8:10 | %FileCheck %s -check-prefix=CHECK-LONG-NUMER

// RUN: %refactor -source-filename %s -pos=10:8 | %FileCheck %s -check-prefix=CHECK-NONE
// RUN: %refactor -source-filename %s -pos=11:8 | %FileCheck %s -check-prefix=CHECK-NONE

// CHECK-LONG-NUMER: Simplify Long Number Literal

// CHECK-NONE: Action begins
// CHECK-NONE-NEXT: Action ends
