// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s
// LValues are direct values, too. They are reference types, though.

func Close(fn: () -> Int) { fn() }
typealias MyFloat = Float

// CHECK: define void @_TF5inout13modifyFooHeap
// CHECK: %[[ALLOCB:.*]] = alloca
// CHECK: %[[ALLOCA:.*]] = alloca
// CHECK: %[[ALLOCABOX:.*]] = alloca
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %[[ALLOCB]]}, metadata ![[B:.*]])
// CHECK:  call void @llvm.dbg.declare(metadata !{{{.*}} %[[ALLOCA]]}, metadata
// CHECK: ![[REFINT:.*]] = {{.*}}[ DW_TAG_reference_type ]{{.*}} [from _TtSi]
// CHECK: ![[B]] = {{.*}}metadata ![[MYFLOAT:.*]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [b] [line [[@LINE+4]]]
// CHECK: ![[MYFLOAT]] = {{.*}}[ DW_TAG_typedef ] [_Tta5inout7MyFloat] {{.*}} [from _TtSf]
// CHECK: ![[A:[0-9]+]] = {{.*}}metadata ![[REFINT]],  i32 0, i32 0} ; [ DW_TAG_arg_variable ] [a] [line [[@LINE+1]]]
func modifyFooHeap(inout a: Int,
                   var b: MyFloat)
{
    if (b > 2.71) {
      a = a + 12// Set breakpoint here
    }

    // Close over the variable to disable promotion of the inout shadow.
    Close({ a })
}

func modifyFoo(inout u: Int,
// CHECK: metadata ![[MYFLOAT]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [v] [line [[@LINE+2]]]
// CHECK: metadata ![[REFINT]], i32 0, i32 0} ; [ DW_TAG_arg_variable ] [u] [line [[@LINE-2]]]
               var v: MyFloat)
{
    if (v > 2.71) {
      u = u - 41
    }
}

func main() -> Int {
    var b = 11 as Int
    modifyFoo(&b, 3.14)

    var c = 64 as Int
    modifyFooHeap(&c, 1.41)
    return 0
}

main()

