// RUN: %target-swift-frontend %use_no_opaque_pointers %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o -
import StdlibUnittest
func foo<T>(_ x: T) -> () {
  // CHECK: define {{.*}} @"$s11generic_arg3fooyyxlF"
  // CHECK: %[[T:.*]] = alloca %swift.type*
  // CHECK: call void @llvm.dbg.declare(metadata %swift.type** %[[T]],
  // CHECK-SAME:               metadata ![[T1:.*]], metadata !DIExpression())
  // CHECK: %[[X:.*]] = alloca %swift.opaque*
  // CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %[[X]],
  // CHECK-SAME: metadata ![[X1:.*]], metadata !DIExpression(DW_OP_deref))
  // CHECK: store %swift.type* %T, %swift.type** %[[T]],
  // CHECK: store %swift.opaque* %0, %swift.opaque** %[[X]],
  // CHECK-DAG: ![[T1]] = !DILocalVariable(name: "$\CF\84_0_0",{{.*}}flags: DIFlagArtificial)
  // CHECK-DAG: ![[X1]] = !DILocalVariable(name: "x", arg: 1,{{.*}}line: 4, type: ![[LET_TY2:[0-9]+]])
  // CHECK-DAG: ![[LET_TY2]] = !DIDerivedType(tag: DW_TAG_const_type,{{.*}}baseType: ![[TY2:[0-9]+]])
  // CHECK-DAG: ![[TY2]] = !DICompositeType({{.*}}name: "$sxD"
  _blackHole(x)
}

foo(42)
