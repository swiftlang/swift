// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
import StdlibUnittest
func foo<T>(_ x: T) -> () {
  // CHECK: define {{.*}} @"$s11generic_arg3fooyyxlF"
  // CHECK: %[[T:.*]] = alloca ptr
  // CHECK: #dbg_declare(ptr %[[T]],
  // CHECK-SAME:               ![[T1:.*]], !DIExpression()
  // CHECK: %[[X:.*]] = alloca ptr
  // CHECK: #dbg_declare(ptr %[[X]],
  // CHECK-SAME: ![[X1:.*]], !DIExpression(DW_OP_deref)
  // CHECK: store ptr %T, ptr %[[T]],
  // CHECK: store ptr %0, ptr %[[X]],
  // CHECK-DAG: ![[T1]] = !DILocalVariable(name: "$\CF\84_0_0",{{.*}}flags: DIFlagArtificial)
  // CHECK-DAG: ![[X1]] = !DILocalVariable(name: "x", arg: 1,{{.*}}line: 3, type: ![[LET_TY2:[0-9]+]])
  // CHECK-DAG: ![[LET_TY2]] = !DIDerivedType(tag: DW_TAG_const_type,{{.*}}baseType: ![[TY2:[0-9]+]])
  // CHECK-DAG: ![[TY2]] = !DICompositeType({{.*}}name: "$sxD"
  _blackHole(x)
}

foo(42)
