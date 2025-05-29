// RUN: %target-swift-frontend -print-ast %s 2>&1 | %FileCheck %s

struct S: ~Copyable {
  consuming func c() {
    discard self
  }

  deinit {}
}

// CHECK:  internal struct S : ~Copyable {
// CHECK:   internal consuming func c() {
// CHECK:     discard self
// CHECK:   }
