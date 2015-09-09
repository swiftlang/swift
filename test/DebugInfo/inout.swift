// RUN: %target-swift-frontend %s -emit-ir -g -module-name inout -o %t.ll
// RUN: cat %t.ll | FileCheck %s
// RUN: cat %t.ll | FileCheck %s --check-prefix=PROMO-CHECK
// RUN: cat %t.ll | FileCheck %s --check-prefix=FOO-CHECK

// LValues are direct values, too. They are reference types, though.

func Close(fn: () -> Int64) { fn() }
typealias MyFloat = Float

// CHECK: define hidden void @_TF5inout13modifyFooHeap
// CHECK: %[[ALLOCB:.*]] = alloca %Sf
// CHECK: %[[ALLOCA:.*]] = alloca %VSs5Int64*
// CHECK-DAG:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCA]], metadata ![[A:[0-9]+]]
// CHECK-DAG:  call void @llvm.dbg.declare(metadata {{.*}} %[[ALLOCB]], metadata ![[B:[0-9]+]], metadata !{{[0-9]+}})

// Closure with promoted capture.
// PROMO-CHECK: define {{.*}}@_TTSf2d_i___TFF5inout13modifyFooHeapFTRVSs5Int64Sf_T_U_FT_S0_
// PROMO-CHECK: call void @llvm.dbg.declare(metadata {{(i32|i64)}}* %{{.*}}, metadata ![[A1:[0-9]+]], metadata ![[EMPTY_EXPR:[0-9]+]])

// PROMO-CHECK-DAG: ![[EMPTY_EXPR]] = !DIExpression()
// PROMO-CHECK-DAG: ![[REFINT:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "_TtRVSs5Int64"
// PROMO-CHECK-DAG: ![[A1]] = !DILocalVariable(name: "a", arg: 1{{.*}} type: !"_TtVSs5Int64"
// PROMO-CHECK-DAG: !DICompositeType(tag: DW_TAG_structure_type, name: "_TtRVSs5Int64"
func modifyFooHeap(inout a: Int64,
// CHECK-DAG: ![[A]] = !DILocalVariable(name: "a", arg: 1{{.*}} line: [[@LINE-1]],{{.*}} type: !"_TtRVSs5Int64"
// CHECK-DAG: ![[B]] = !DILocalVariable(name: "b", arg: 2{{.*}} line: [[@LINE+2]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
// CHECK-DAG: ![[MYFLOAT]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
                   var _ b: MyFloat)
{
    if (b > 2.71) {
      a = a + 12// Set breakpoint here
    }

    // Close over the variable to disable promotion of the inout shadow.
    Close({ a })
}

// Inout reference type.
// FOO-CHECK: define {{.*}}@_TF5inout9modifyFooFTRVSs5Int64Sf_T_
// FOO-CHECK: call void @llvm.dbg.declare(metadata %VSs5Int64** %{{.*}}, metadata ![[U:[0-9]+]], metadata ![[EMPTY_EXPR:.*]])
// FOO-CHECK-DAG: ![[EMPTY_EXPR]] = !DIExpression()
func modifyFoo(inout u: Int64,
// FOO-CHECK-DAG: !DILocalVariable(name: "v", arg: 2{{.*}} line: [[@LINE+2]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
// FOO-CHECK-DAG: [[U]] = !DILocalVariable(name: "u", arg: 1{{.*}} line: [[@LINE-2]],{{.*}} type: !"_TtRVSs5Int64"
               var _ v: MyFloat)
// FOO-CHECK-DAG: ![[MYFLOAT]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
{
    if (v > 2.71) {
      u = u - 41
    }
}

func main() -> Int64 {
    var c = 11 as Int64
    modifyFoo(&c, 3.14)

    var d = 64 as Int64
    modifyFooHeap(&d, 1.41)
    return 0
}

main()

