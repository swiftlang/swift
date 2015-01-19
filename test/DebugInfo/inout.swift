// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

// LValues are direct values, too. They are reference types, though.

func Close(fn: () -> Int) { fn() }
typealias MyFloat = Float

// CHECK: define hidden void @_TF5inout13modifyFooHeap
// CHECK: %[[ALLOCB:.*]] = alloca
// CHECK: %[[ALLOCA:.*]] = alloca
// CHECK: %[[ALLOCABOX:.*]] = alloca
// CHECK:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCB]], metadata ![[B:.*]], metadata !{{[0-9]+}})
// CHECK:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCA]], metadata

// Closure with promoted capture.
// CHECK: define {{.*}}@_TTSf2d_i___TFF5inout13modifyFooHeapFTRSiSf_T_U_FT_Si
// CHECK: call void @llvm.dbg.declare(metadata {{(i32|i64)}}* %{{.*}}, metadata ![[A1:[0-9]+]], metadata ![[EMPTY_EXPR:[0-9]+]])

// Inout reference type.
// CHECK: define {{.*}}@_TF5inout9modifyFooFTRSiSf_T_
// CHECK: call void @llvm.dbg.declare(metadata %Si** %{{.*}}, metadata ![[U:[0-9]+]], metadata ![[EMPTY_EXPR]])

// CHECK-DAG: ![[EMPTY_EXPR]] = !{!"0x102"} ; [ DW_TAG_expression ]
// CHECK-DAG: ![[REFINT:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtRSi]
// CHECK-DAG: ![[A1]] = {{.*}}!"_TtSi"} ; [ DW_TAG_arg_variable ] [a]
func modifyFooHeap(inout a: Int,
// CHECK-DAG: ![[A:[0-9]+]] = {{.*}}!"_TtRSi"} ; [ DW_TAG_arg_variable ] [a] [line [[@LINE-1]]]
// CHECK-DAG: ![[B]] = {{.*}}![[MYFLOAT:.*]]} ; [ DW_TAG_arg_variable ] [b] [line [[@LINE+2]]]
// CHECK-DAG: ![[MYFLOAT]] = {{.*}}[ DW_TAG_typedef ] [_Tta5inout7MyFloat] {{.*}} [from _TtSf]
                   var b: MyFloat)
{
    if (b > 2.71) {
      a = a + 12// Set breakpoint here
    }

    // Close over the variable to disable promotion of the inout shadow.
    Close({ a })
}

func modifyFoo(inout u: Int,
// CHECK-DAG: ![[MYFLOAT]]} ; [ DW_TAG_arg_variable ] [v] [line [[@LINE+2]]]
// CHECK-DAG: [[U]] = {{.*}}!"_TtRSi"} ; [ DW_TAG_arg_variable ] [u] [line [[@LINE-2]]]
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

