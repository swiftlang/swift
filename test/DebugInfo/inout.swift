// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
// FIXME--needs to be updated for inout writeback

// CHECK: define void @_T5inout9modifyFooFT1aRSi1bSf_T_
// CHECK: %[[ALLOCB:.*]] = alloca
// CHECK: %[[ALLOCA:.*]] = alloca
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %[[ALLOCB]]}, metadata ![[B:.*]])
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %[[ALLOCA]]}, metadata ![[A:.*]])
// CHECK: ![[REFSI:.*]] = {{.*}}[ DW_TAG_reference_type ]{{.*}} [from _TtSi]
// CHECK-DAG: ![[SF:.*]] = {{.*}}[ DW_TAG_typedef ] [Float]
// CHECK-DAG: ![[B]] = {{.*}}metadata ![[SF]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [b] [line
// FIXME: The line number points to the first use, but should point to the parameter declaration.
func modifyFoo (a : [inout] Int,
// CHECK-DAG: ![[A]] = {{.*}}metadata ![[REFSI]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [a] [line
// FIXME: The line number points to the first use, but should point to the parameter declaration.
                b : Float) {
    if (b > 2.89) {
      a = a + 12// Set breakpoint here
    }
}

func main() -> Int {
    var b = Int(11)
    modifyFoo(&b, 3.14)
    return 0
}

main()

