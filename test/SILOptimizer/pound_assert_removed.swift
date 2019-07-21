// RUN: %target-swift-frontend -enable-experimental-static-assert -emit-silgen %s | %target-sil-opt -irgen-prepare | %FileCheck %s

// Tests that mandatory SIL passes remove the builtin poundAssert instruction.
// CHECK-LABEL: pound_assert_removed14builtinRemoved
public func builtinRemoved() {
  #assert(true)
  // CHECK-NOT: builtin "poundAssert"
}
