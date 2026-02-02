// RUN: not %target-build-swift %S/Inputs/error.swift %s 2>&1 | %FileCheck %s
// RUN: not %target-build-swift -continue-building-after-errors %S/Inputs/error.swift %s 2>&1 | %FileCheck -check-prefix=CHECK-CONTINUE %s

// CHECK: self.bar = self.bar
// CHECK-NOT: self.baz = self.baz
// CHECK-CONTINUE: self.bar = self.bar
// CHECK-CONTINUE: self.baz = self.baz
// CHECK-BATCH-DAG: self.bar = self.bar
// CHECK-BATCH-DAG: self.baz = self.baz
struct Bar {
  let baz: Int
  init() {
    self.baz = self.baz
  }
}
