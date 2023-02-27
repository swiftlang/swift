// RUN: %target-swift-frontend -emit-ir %s -g -o - \
// RUN:    -enable-experimental-feature VariadicGenerics \
// RUN:    -parse-as-library -module-name a | %FileCheck %s

// Because of -enable-experimental-feature VariadicGenerics
// REQUIRES: asserts

public func foo<T...>(args: repeat each T) {
  // CHECK: define {{.*}} @"$s1a3foo4argsyxxQp_tRvzlF"
  // CHECK-SAME: %swift.type** %[[TYPE_PACK_ARG:.*]])
  // CHECK: %[[TYPE_PACK_ALLOCA:.*]] = alloca %swift.type**
  // CHECK: call void @llvm.dbg.declare(metadata %swift.type*** %[[TYPE_PACK_ALLOCA]], metadata ![[TYPE_PACK_VAR:[0-9]+]], metadata !DIExpression())
  // CHECK: %[[ARGS_ALLOCA:.*]] = alloca %swift.opaque**
  // CHECK-DAG: call void @llvm.dbg.declare(metadata %swift.opaque*** %[[ARGS_ALLOCA]], metadata ![[ARGS_VAR:[0-9]+]], metadata !DIExpression(DW_OP_deref))
  // CHECK-DAG: store %swift.type** %[[TYPE_PACK_ARG]], %swift.type*** %[[TYPE_PACK_ALLOCA]]
  // CHECK-DAG: store %swift.opaque** %0, %swift.opaque*** %[[ARGS_ALLOCA]]
  // CHECK-DAG: ![[ARGS_VAR]] = !DILocalVariable(name: "args", arg: 1, {{.*}}line: [[@LINE-9]], type: ![[ARGS_LET_TY:[0-9]+]])
  // CHECK-DAG: ![[ARGS_LET_TY]] = !DIDerivedType(tag: DW_TAG_const_type, baseType: ![[ARGS_TY:[0-9]+]])
  // CHECK-DAG: ![[ARGS_TY]] = !DICompositeType({{.*}}identifier: "$sxxQp_QSiD")
  // CHECK-DAG: ![[TYPE_PACK_VAR]] = !DILocalVariable(name: "$\CF\84_0_0", {{.*}}type: ![[TYPE_PACK_TYD:[0-9]+]], flags: DIFlagArtificial)
  // CHECK-DAG: ![[TYPE_PACK_TYD]] = !DIDerivedType(tag: DW_TAG_typedef, name: "T", {{.*}}baseType: ![[TYPE_PACK_TY:[0-9]+]]
  // CHECK-DAG: ![[TYPE_PACK_TY]] = !DIDerivedType(tag: DW_TAG_pointer_type, name: "$sBpD"
}

