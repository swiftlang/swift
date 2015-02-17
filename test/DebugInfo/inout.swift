// RUN: %target-swift-frontend %s -emit-ir -g -module-name inout -o %t.ll
// RUN: cat %t.ll | FileCheck %s
// RUN: cat %t.ll | FileCheck %s --check-prefix=PROMO-CHECK
// RUN: cat %t.ll | FileCheck %s --check-prefix=FOO-CHECK

// LValues are direct values, too. They are reference types, though.

func Close(fn: () -> Int) { fn() }
typealias MyFloat = Float

// CHECK: define hidden void @_TF5inout13modifyFooHeap
// CHECK: %[[ALLOCB:.*]] = alloca %Sf
// CHECK: %[[ALLOCA:.*]] = alloca %Si*
// CHECK-DAG:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCA]], metadata ![[A:[0-9]+]]
// CHECK-DAG:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCB]], metadata ![[B:[0-9]+]], metadata !{{[0-9]+}})

// Closure with promoted capture.
// PROMO-CHECK: define {{.*}}@_TTSf2d_i___TFF5inout13modifyFooHeapFTRSiSf_T_U_FT_Si
// PROMO-CHECK: call void @llvm.dbg.declare(metadata {{(i32|i64)}}* %{{.*}}, metadata ![[A1:[0-9]+]], metadata ![[EMPTY_EXPR:[0-9]+]])

// PROMO-CHECK-DAG: ![[EMPTY_EXPR]] = !{!"0x102"} ; [ DW_TAG_expression ]
// PROMO-CHECK-DAG: ![[REFINT:.*]] = {{.*}}[ DW_TAG_structure_type ] [_TtRSi]
// PROMO-CHECK-DAG: ![[A1]] = {{.*}}!"_TtSi"} ; [ DW_TAG_arg_variable ] [a]
// PROMO-CHECK-DAG: "_TtRSi"{{.*}}[ DW_TAG_structure_type ] [_TtRSi] [line 0,
func modifyFooHeap(inout a: Int,
// CHECK-DAG: ![[A]] = {{.*}}!"_TtRSi"} ; [ DW_TAG_arg_variable ] [a] [line [[@LINE-1]]]
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

// Inout reference type.
// FOO-CHECK: define {{.*}}@_TF5inout9modifyFooFTRSiSf_T_
// FOO-CHECK: call void @llvm.dbg.declare(metadata %Si** %{{.*}}, metadata ![[U:[0-9]+]], metadata ![[EMPTY_EXPR:.*]])
// FOO-CHECK-DAG: ![[EMPTY_EXPR]] = !{!"0x102"} ; [ DW_TAG_expression ]
func modifyFoo(inout u: Int,
// FOO-CHECK-DAG: ![[MYFLOAT:[0-9]+]]} ; [ DW_TAG_arg_variable ] [v] [line [[@LINE+2]]]
// FOO-CHECK-DAG: [[U]] = {{.*}}!"_TtRSi"} ; [ DW_TAG_arg_variable ] [u] [line [[@LINE-2]]]
               var v: MyFloat)
// CHECK-DAG: ![[MYFLOAT]] = {{.*}}[ DW_TAG_typedef ] [_Tta5inout7MyFloat] {{.*}} [from _TtSf]
{
    if (v > 2.71) {
      u = u - 41
    }
}

func main() -> Int {
    var c = 11 as Int
    modifyFoo(&c, 3.14)

    var d = 64 as Int
    modifyFooHeap(&d, 1.41)
    return 0
}

main()

