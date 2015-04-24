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

// PROMO-CHECK-DAG: ![[EMPTY_EXPR]] = !MDExpression()
// PROMO-CHECK-DAG: ![[REFINT:.*]] = !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtRSi"
// PROMO-CHECK-DAG: ![[A1]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "a",{{.*}} type: !"_TtSi"
// PROMO-CHECK-DAG: !MDCompositeType(tag: DW_TAG_structure_type, name: "_TtRSi"
func modifyFooHeap(inout a: Int,
// CHECK-DAG: ![[A]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "a",{{.*}} line: [[@LINE-1]],{{.*}} type: !"_TtRSi"
// CHECK-DAG: ![[B]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "b",{{.*}} line: [[@LINE+2]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
// CHECK-DAG: ![[MYFLOAT]] = !MDDerivedType(tag: DW_TAG_typedef, name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
                   var _ b: MyFloat)
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
// FOO-CHECK-DAG: ![[EMPTY_EXPR]] = !MDExpression()
func modifyFoo(inout u: Int,
// FOO-CHECK-DAG: !MDLocalVariable(tag: DW_TAG_arg_variable, name: "v",{{.*}} line: [[@LINE+2]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
// FOO-CHECK-DAG: [[U]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "u",{{.*}} line: [[@LINE-2]],{{.*}} type: !"_TtRSi"
               var _ v: MyFloat)
// FOO-CHECK-DAG: ![[MYFLOAT]] = !MDDerivedType(tag: DW_TAG_typedef, name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
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

