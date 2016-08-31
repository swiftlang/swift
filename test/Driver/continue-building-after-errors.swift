// RUN: not %swiftc_driver %S/Inputs/error.swift %s 2>&1 | %FileCheck %s
// RUN: not %swiftc_driver -continue-building-after-errors %S/Inputs/error.swift %s 2>&1 | %FileCheck -check-prefix=CHECK-CONTINUE %s

// CHECK: self.bar = self.bar
// CHECK-NOT: self.baz = self.baz
// CHECK-CONTINUE: self.bar = self.bar
// CHECK-CONTINUE: self.baz = self.baz
struct Bar {
  let baz: Int
  init() {
    self.baz = self.baz
  }
}
