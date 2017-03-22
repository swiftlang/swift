// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
import StdlibUnittest
func foo<T>(_ x: T) -> () {
  // CHECK: define {{.*}} @_T011generic_arg3fooyxlF
  // CHECK: %[[T:.*]] = alloca %swift.type*
  // CHECK: %[[X:.*]] = alloca %swift.opaque*
  // CHECK: store %swift.type* %T, %swift.type** %[[T]],
  // CHECK: call void @llvm.dbg.declare(metadata %swift.type** %[[T]],
  // CHECK-SAME:               metadata ![[T1:.*]], metadata ![[EMPTY:.*]])
  // CHECK: store %swift.opaque* %0, %swift.opaque** %[[X]],
  // CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %[[X]],
  // CHECK-SAME:               metadata ![[X1:.*]], metadata ![[EMPTY]])
  // CHECK: ![[T1]] = !DILocalVariable(name: "$swift.type.T",
  // CHECK-SAME:                       flags: DIFlagArtificial)
  // CHECK: ![[EMPTY]] = !DIExpression()
  // CHECK: ![[X1]] = !DILocalVariable(name: "x", arg: 1,
  // CHECK-SAME:          line: 3, type: ![[TY:.*]])
  // CHECK: ![[TY]] = !DICompositeType({{.*}}identifier: "_T011generic_arg3fooyxlFQq_D")
  _blackHole(x)
}

foo(42)
