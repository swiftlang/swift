// RUN: %target-swift-frontend -emit-sil %s -verify | %target-sil-opt -cleanup | %FileCheck %s

// Tests that mandatory SIL passes remove the builtin poundAssert instruction
// and emit a diagnostic.
// CHECK-LABEL: sil @$S12pound_assert14builtinRemoved{{[_0-9a-zA-Z]*}}
public func builtinRemoved() {
  #assert(true) // expected-warning {{#assert doesn't actually do anything yet}}
  // CHECK-NOT: builtin "poundAssert"
}
