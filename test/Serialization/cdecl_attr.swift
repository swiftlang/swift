// RUN: %empty-directory(%t)
// Ensure .swift -> .ll
// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// Ensure .swift -> .sib -> .ll
// RUN: %target-swift-frontend -emit-sib %s -o %t/cdecl_attr.sib
// RUN: %target-swift-frontend -emit-ir %t/cdecl_attr.sib | %FileCheck %s

// CHECK: define hidden {{.*}} @foo

@_cdecl("foo")
func foo() {}
