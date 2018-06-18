// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func apply<Type>(_ T : Type, fn: (Type) -> Type) -> Type { return fn(T) }

public func f<Type>(_ value : Type)
{
  // CHECK: define {{.*}}$S12generic_arg31fyyxlFxxXEfU_
  // CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %[[ALLOCA:[^,]+]],
  // CHECK-SAME:       metadata ![[ARG:.*]], metadata !DIExpression())
  // CHECK: store %swift.opaque* %1, %swift.opaque** %[[ALLOCA]], align
  // No deref here.
  // CHECK: ![[TY:.*]] = !DICompositeType({{.*}}identifier: "$SxD"
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:                        line: [[@LINE+1]], type: ![[TY]])
  apply(value) { arg in return arg }
}
