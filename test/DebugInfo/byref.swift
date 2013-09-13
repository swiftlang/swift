// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// CHECK: define void @_T5byref9modifyFooFT1aRSi1bSf_T_
// CHECK: %[[ALLOCA:.*]] = alloca
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %[[ALLOCA]]}, metadata ![[A:.*]])
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %0}, metadata ![[B:.*]])
// CHECK: ![[SI:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtSi]
// CHECK-DAG: ![[SF:.*]] = {{.*}}[ DW_TAG_typedef ] [Float]
// CHECK-DAG: ![[A]] = {{.*}}metadata ![[SF]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [b] [line
// FIXME: The line number points to the first use, but should point to the parameter declaration.
func modifyFoo (a : [byref] Int,
// CHECK-DAG: ![[B]] = {{.*}}metadata ![[SI]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] {{.*}}[line
// FIXME: There is no name.
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

