// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

func apply<Type>(_ T : Type, fn: (Type) -> Type) -> Type { return fn(T) }

public func f<Type>(_ value : Type)
{
  // CHECK: define {{.*}}_T012generic_arg31fyxlFxxcfU_
  // CHECK: store %swift.opaque* %1, %swift.opaque** %[[ALLOCA:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata %swift.opaque** %[[ALLOCA]],
  // CHECK-SAME:       metadata ![[ARG:.*]], metadata ![[EXPR:.*]])
  // No deref here: The argument is an Archetype and this implicitly indirect.
  // CHECK: ![[EXPR]] = !DIExpression()
  // CHECK: ![[TY:.*]] = !DICompositeType({{.*}}identifier: "_T012generic_arg31fyxlFQq_D"
  // CHECK: ![[ARG]] = !DILocalVariable(name: "arg", arg: 1,
  // CHECK-SAME:                        line: [[@LINE+1]], type: ![[TY]])
  apply(value) { arg in return arg }
}
