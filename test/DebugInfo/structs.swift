// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

// This is more of a crash test than anything else.

struct A {
  var fn : () -> ()
}

func test(var x : A) {}
// CHECK:    define hidden void @_TF7structs4test
// CHECK:      [[X:%.*]] = alloca [[A:%.*]], align {{(4|8)}}
// CHECK-NEXT: call void @llvm.dbg.declare(metadata [[A]]* [[X]], metadata [[X_DBG:!.*]], metadata !{{[0-9]+}})
// CHECK: ![[A_DI:[^,]+]]} ; [ DW_TAG_structure_type ] [A] [line [[@LINE-8]]
// CHECK: [[X_DBG]] = {{.*}}![[A_DI]]} ; [ DW_TAG_arg_variable ] [x] [line [[@LINE-5]]]
