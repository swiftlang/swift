// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
// REQUIRES: objc_interop
public struct Q<T> {
  let x: T
}
// CHECK: define {{.*}}$S12generic_arg43fooyySayAA1QVyxGGlF
// CHECK: call void @llvm.dbg.declare
// CHECK: call void @llvm.dbg.declare(metadata %[[TY:.*]]** %[[ALLOCA:[^,]+]],
// CHECK-SAME:       metadata ![[ARG:.*]], metadata !DIExpression())
// CHECK: store %[[TY]]* %0, %[[TY]]** %[[ALLOCA]], align
// No deref here: the array argument is passed by value.
// CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
// CHECK-SAME:                        line: [[@LINE+2]], type: ![[TY:.*]])
// CHECK: ![[TY]] = !DICompositeType({{.*}}identifier: "$SSay12generic_arg41QVyAA3fooyySayACyxGGlFQq_GGD")
public func foo<T>(_ arg: [Q<T>]) {
}
