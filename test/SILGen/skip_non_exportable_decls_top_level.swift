// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %s -module-name Test | %FileCheck %s --check-prefixes=CHECK,CHECK-NO-SKIP
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %s -module-name Test -experimental-skip-non-exportable-decls | %FileCheck %s --check-prefixes=CHECK,CHECK-SKIP

// CHECK-NO-SKIP: sil_global hidden @$s4Test1xSivp : $Int
// CHECK-SKIP: sil_global hidden_external @$s4Test1xSivp : $Int
var x = foo()

// CHECK-NO-SKIP: sil hidden{{.*}} @$s4Test3fooSiyF : $@convention(thin) () -> Int {
// CHECK-SKIP: sil hidden_external @$s4Test3fooSiyF : $@convention(thin) () -> Int
func foo() -> Int { return 1 }
