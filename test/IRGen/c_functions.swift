// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -import-objc-header %S/Inputs/c_functions.h -primary-file %s -emit-ir | %FileCheck %s

// This is deliberately not a SIL test so that we can test SILGen too.

// CHECK-LABEL: define hidden swiftcc void @_T011c_functions14testOverloadedyyF
func testOverloaded() {
  // CHECK: call void @_Z10overloadedv()
  overloaded()
  // CHECK: call void @_Z10overloadedi(i32{{( signext)?}} 42)
  overloaded(42)
  // CHECK: call void @{{.*}}test_my_log
  test_my_log()
} // CHECK: {{^}$}}
