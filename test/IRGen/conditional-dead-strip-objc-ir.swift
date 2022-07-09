// Tests that with -conditional-runtime-records, IRGen marks class, struct,
// enum, protocol, and protocol conformance records as conditionally removable
// via !llvm.used.conditional metadata.

// REQUIRES: objc_interop

// RUN: %target-build-swift -Xfrontend -conditional-runtime-records %s -emit-ir -o - | %FileCheck %s

public class Class {
}

// CHECK:      @llvm.{{(compiler.)?}}used = appending global [
// CHECK-SAME:   @"$s4main5ClassCHn"
// CHECK-SAME: ], section "llvm.metadata"

// CHECK:      !llvm.used.conditional = !{[[M1:!.*]], [[M2:!.*]], [[O1:!.*]]}

// CHECK-DAG:      [[M1]]  = !{{{.*}} @"$s4main5ClassCMF", i32 0, [[M1A:!.*]]}
// CHECK-DAG:      [[M1A]] = !{{{.*}} @"$s4main5ClassCMn"
// CHECK-DAG:      [[M2]]  = !{{{.*}} @"$s4main5ClassCHn", i32 0, [[M1A:!.*]]}

// CHECK-DAG:      [[O1]]  = !{{{.*}} @"objc_classes_$s4main5ClassCN", i32 0, [[O1A:!.*]]}
// CHECK-DAG:      [[O1A]] = !{{{.*}} @"$s4main5ClassCN"}
