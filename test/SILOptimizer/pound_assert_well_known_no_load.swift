// RUN: %target-swift-frontend -enable-experimental-static-assert -emit-sil %s -verify | %FileCheck %s

// Checks that the constant evaluator does not load WellKnownFunctions. This
// test is in its own file because other test code might load
// WellKnownFunctions for valid reasons.

struct IntStringPair {
  var i: Int
  var s: String
}

func testStringInit() {
  // String initialization is a WellKnownFunction.
  #assert(IntStringPair(i: 0, s: "").i == 0)
}

// String.init should appear as a declaration, but not a definition.
// CHECK-LABEL: // String.init
// CHECK-NOT: bb0
// The following check for empty line finds the end of the String.init decl/def,
// preventing the bb0 check-not from finding a bb0 in a subsequent def.
// CHECK: {{^$}}
