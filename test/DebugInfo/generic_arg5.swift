// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
public struct S<Type>
{
  let value : Type
}

public func foo<Type>(_ values : [S<Type>])
{
  // CHECK: define {{.*}}$S12generic_arg53fooyySayAA1SVyxGGlFAESgAEXEfU_
  // CHECK: call void @llvm.dbg.declare
  // CHECK: call void @llvm.dbg.declare(metadata %[[TY:.*]]** %[[ALLOCA:[^,]+]],
  // CHECK-SAME:       metadata ![[ARG:[0-9]+]],
  // CHECK-SAME:       metadata !DIExpression(DW_OP_deref))
  // CHECK: store %[[TY]]* %1, %[[TY]]** %[[ALLOCA]], align
  // The argument is a by-ref struct and thus needs to be dereferenced.
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:                        line: [[@LINE+4]],
  // CHECK-SAME:     type: ![[TY:.*]])
  // CHECK: ![[TY]] = !DICompositeType(
  // CHECK-SAME:              identifier: "$S12generic_arg51SVyxGD")
  let _ = values.flatMap { arg in
    return .some(arg)
  }
 
}
