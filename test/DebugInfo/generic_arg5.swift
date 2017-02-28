// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s
public struct S<Type>
{
  let value : Type
}

public func foo<Type>(_ values : [S<Type>])
{
  // CHECK: define {{.*}}_T012generic_arg53fooySayAA1SVyxGGlFADyxGSgADyxGcfU_
  // CHECK: store %[[TY:.*]]* %1, %[[TY]]** %[[ALLOCA:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %[[TY]]** %[[ALLOCA]],
  // CHECK-SAME:       metadata ![[ARG:.*]], metadata ![[EXPR:.*]])
  // The argument is a by-ref struct and thus needs to be dereferenced.
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:                        line: [[@LINE+4]],
  // CHECK-SAME:     type: ![[TY:.*]])
  // CHECK: ![[TY]] = !DICompositeType({{.*}}identifier: "_T012generic_arg51SVyAA3fooySayACyxGGlFQq_GD")
  // CHECK: ![[EXPR]] = !DIExpression(DW_OP_deref)
  let _ = values.flatMap { arg in
    return .some(arg)
  }
 
}
