// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
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
  // CHECK: ![[TY2:.*]] = !DICompositeType({{.*}}identifier: "$sxD")
  // CHECK: ![[T1]] = !DILocalVariable(name: "$\CF\84_0_0",
  // CHECK-SAME:                       flags: DIFlagArtificial)
  // CHECK: ![[X1]] = !DILocalVariable(name: "x", arg: 1,
  // CHECK-SAME:          line: 3, type: ![[TY2]])
  _blackHole(x)
}

foo(42)
