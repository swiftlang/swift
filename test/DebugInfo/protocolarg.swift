// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(_ t: T) {}

public protocol IGiveOutInts {
  func callMe() -> Int64
}

// CHECK: define {{.*}}@_TF11protocolarg16printSomeNumbersFPS_12IGiveOutInts_T_
// CHECK: @llvm.dbg.declare(metadata %P11protocolarg12IGiveOutInts_** %
// CHECK-SAME:              metadata ![[ARG:[0-9]+]], metadata ![[DEREF:[0-9]+]])
// CHECK: @llvm.dbg.declare(metadata %P11protocolarg12IGiveOutInts_* %
// CHECK-SAME:              metadata ![[VAR:[0-9]+]], metadata ![[EMPTY:[0-9]+]])

public func printSomeNumbers(_ gen: IGiveOutInts) {
  var gen = gen
  // CHECK-DAG: ![[EMPTY]] = !DIExpression()
  // CHECK-DAG: ![[ARG]] = !DILocalVariable(name: "gen", arg: 1, {{.*}}, line: [[@LINE-3]], type: ![[PT:[0-9]+]]
  // CHECK-DAG: ![[VAR]] = !DILocalVariable(name: "gen", {{.*}} line: [[@LINE-3]]
  // CHECK-DAG: ![[DEREF]] = !DIExpression(DW_OP_deref)
  markUsed(gen.callMe())
}

// FIXME: Should be DW_TAG_interface_type
// CHECK-DAG: ![[PT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "IGiveOutInts"

