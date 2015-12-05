// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func apply<Type>(T : Type, fn: (Type) -> Type) -> Type { return fn(T) }

public func f<Type>(value : Type)
{
  // CHECK: define {{.*}}_TFF12generic_arg31furFxT_U_FQ_Q_
  // CHECK: store %swift.opaque* %1, %swift.opaque** %[[ALLOCA:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %[[ALLOCA]],
  // CHECK-SAME:       metadata ![[ARG:.*]], metadata ![[EXPR:.*]])
  // A generic argument is implictly indirect and should not be dereferenced.
  // CHECK: ![[EXPR]] = !DIExpression()
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:     line: [[@LINE+1]], type: !"_TtQq_F12generic_arg31furFxT_")
  apply(value) { arg in return arg }
}
