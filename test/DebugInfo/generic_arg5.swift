// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public struct S<Type>
{
  let value : Type
}

public func foo<Type>(_ values : [S<Type>])
{
  // CHECK: define {{.*}}_TFF12generic_arg53foourFGSaGVS_1Sx__T_U_FGS0_Q__GSqGS0_Q___
  // CHECK: store %[[TY:.*]]* %1, %[[TY]]** %[[ALLOCA:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %[[TY]]** %[[ALLOCA]],
  // CHECK-SAME:       metadata ![[ARG:.*]], metadata ![[EXPR:.*]])
  // The argument is a by-ref struct and thus needs to be dereferenced.
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:                        line: [[@LINE+4]],
  // CHECK-SAME:     type: ![[TY:.*]])
  // CHECK: ![[TY]] = !DICompositeType({{.*}}identifier: "_TtGV12generic_arg51SQq_FS_3foourFGSaGS0_x__T__")
  // CHECK: ![[EXPR]] = !DIExpression(DW_OP_deref)
  let _ = values.flatMap { arg in
    return arg
  }
 
}
