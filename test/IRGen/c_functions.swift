// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -import-objc-header %S/Inputs/c_functions.h -primary-file %s -emit-ir | %FileCheck %s

// This is deliberately not a SIL test so that we can test SILGen too.

// CHECK-LABEL: define hidden void @_TF11c_functions14testOverloadedFT_T_
func testOverloaded() {
  // CHECK: call void @_Z10overloadedv()
  overloaded()
  // CHECK: call void @_Z10overloadedi(i32{{( signext)?}} 42)
  overloaded(42)
} // CHECK: {{^}$}}
