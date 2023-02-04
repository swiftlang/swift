// RUN: %target-swift-frontend -emit-ir %s -g -o - \
// RUN:    -enable-experimental-feature VariadicGenerics \
// RUN:    -parse-as-library -module-name a \
// RUN:    -disable-round-trip-debug-types | %FileCheck %s
// FIXME:  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ remove this!

public func foo<T...>(args: repeat each T) {
  // CHECK: define {{.*}} @"$s1a3foo4argsyxxQp_tlF"
  // CHECK: %[[ARGS_ALLOCA:.*]] = alloca %swift.opaque**
  // CHECK-DAG: call void @llvm.dbg.declare(metadata %swift.opaque*** %[[ARGS_ALLOCA]], metadata ![[ARGS_VAR:[0-9]+]], metadata !DIExpression(DW_OP_deref))
  // CHECK-DAG: store %swift.opaque** %0, %swift.opaque*** %[[ARGS_ALLOCA]]
  // CHECK-DAG: ![[ARGS_VAR]] = !DILocalVariable(name: "args", arg: 1, {{.*}}line: [[@LINE-5]], type: ![[ARGS_LET_TY:[0-9]+]])
  // CHECK-DAG: ![[ARGS_LET_TY]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[ARGS_TY:[0-9]+]])
  // CHECK-DAG: ![[ARGS_TY]] = !DICompositeType({{.*}}identifier: "$sxxQp_QSiD")
}

