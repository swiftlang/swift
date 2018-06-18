// RUN: %target-swift-frontend -emit-silgen %s | %target-sil-opt -irgen-prepare | %FileCheck %s

// Tests that mandatory SIL passes remove the builtin poundAssert instruction.
// CHECK-LABEL: sil @$S20pound_assert_removed14builtinRemoved{{[_0-9a-zA-Z]*}}
public func builtinRemoved() {
  #assert(true)
  // CHECK-NOT: builtin "poundAssert"
}
