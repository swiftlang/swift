// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

struct S : ~Copyable {
  __consuming func c() {
    _forget self
  }
}

// CHECK: internal struct S : ~Copyable {
// CHECK:   internal __consuming func c() {
// CHECK:     _forget self
// CHECK:   }
