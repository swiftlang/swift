// RUN: %target-swift-frontend %s -emit-ir -g -module-name inout -o %t.ll
// RUN: cat %t.ll | FileCheck %s
// RUN: cat %t.ll | FileCheck %s --check-prefix=PROMO-CHECK
// RUN: cat %t.ll | FileCheck %s --check-prefix=FOO-CHECK

// LValues are direct values, too. They are reference types, though.

func Close(fn: () -> Int64) { fn() }
typealias MyFloat = Float

// CHECK: define hidden void @_TF5inout13modifyFooHeap
// CHECK: %[[ALLOCA:.*]] = alloca %Vs5Int64*
// CHECK: %[[ALLOCB:.*]] = alloca %Sf
// CHECK: call void @llvm.dbg.declare(metadata
// CHECK-SAME:                        %[[ALLOCA]], metadata ![[A:[0-9]+]]
// CHECK: call void @llvm.dbg.declare(metadata
// CHECK-SAME:       %[[ALLOCB]], metadata ![[B:[0-9]+]], metadata !{{[0-9]+}})

// Closure with promoted capture.
// PROMO-CHECK: define {{.*}}@_TTSf2i___TFF5inout13modifyFooHeapFTRVs5Int64Sf_T_U_FT_S0_
// PROMO-CHECK: call void @llvm.dbg.declare(metadata {{(i32|i64)}}* %
// PROMO-CHECK-SAME:   metadata ![[A1:[0-9]+]], metadata ![[EMPTY_EXPR:[0-9]+]])

// PROMO-CHECK: ![[EMPTY_EXPR]] = !DIExpression()
// PROMO-CHECK: ![[A1]] = !DILocalVariable(name: "a", arg: 1
// PROMO-CHECK-SAME:                       type: !"_TtVs5Int64"
func modifyFooHeap(inout a: Int64,
  // CHECK: ![[A]] = !DILocalVariable(name: "a", arg: 1
  // CHECK-SAME:         line: [[@LINE-2]],{{.*}} type: !"_TtRVs5Int64"
  // CHECK: ![[B]] = !DILocalVariable(name: "b", scope:
  // CHECK-SAME:         line: [[@LINE+5]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
  // CHECK: ![[MYFLOAT]] = !DIDerivedType(tag: DW_TAG_typedef,
  // CHECK-SAME:           name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
                   _ b: MyFloat)
{
    var b = b
    if (b > 2.71) {
      a = a + 12// Set breakpoint here
    }

    // Close over the variable to disable promotion of the inout shadow.
    Close({ a })
}

// Inout reference type.
// FOO-CHECK: define {{.*}}@_TF5inout9modifyFooFTRVs5Int64Sf_T_
// FOO-CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64** %
// FOO-CHECK-SAME:          metadata ![[U:[0-9]+]], metadata ![[EMPTY_EXPR:.*]])
// FOO-CHECK: ![[EMPTY_EXPR]] = !DIExpression()
func modifyFoo(inout u: Int64,
// FOO-CHECK-DAG: !DILocalVariable(name: "v", scope:{{.*}} line: [[@LINE+5]],{{.*}} type: ![[MYFLOAT:[0-9]+]]
// FOO-CHECK-DAG: [[U]] = !DILocalVariable(name: "u", arg: 1{{.*}} line: [[@LINE-2]],{{.*}} type: !"_TtRVs5Int64"
               _ v: MyFloat)
// FOO-CHECK-DAG: ![[MYFLOAT]] = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tta5inout7MyFloat",{{.*}} baseType: !"_TtSf"
{
    var v = v
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

