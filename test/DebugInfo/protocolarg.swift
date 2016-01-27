// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

public protocol IGiveOutInts {
  func callMe() -> Int64
}

// CHECK: define {{.*}}@_TF11protocolarg16printSomeNumbersFPS_12IGiveOutInts_T_
// CHECK: @llvm.dbg.declare(metadata %P11protocolarg12IGiveOutInts_* %
// CHECK-SAME:              metadata ![[VAR:.*]], metadata ![[EMPTY:.*]])
// CHECK: @llvm.dbg.declare(metadata %P11protocolarg12IGiveOutInts_** %
// CHECK-SAME:              metadata ![[ARG:.*]], metadata ![[DEREF:.*]])

// FIXME: Should be DW_TAG_interface_type
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "IGiveOutInts"
// CHECK-SAME:             identifier: [[PT:"[^"]+"]]

public func printSomeNumbers(gen: IGiveOutInts) {
  var gen = gen
  // CHECK: ![[EMPTY]] = !DIExpression()
  // CHECK: ![[VAR]] = !DILocalVariable(name: "gen", {{.*}} line: [[@LINE-2]]
  // CHECK: ![[ARG]] = !DILocalVariable(name: "gen", arg: 1,
  // CHECK-SAME:                        line: [[@LINE-5]], type: ![[PT]]
  // CHECK: ![[DEREF]] = !DIExpression(DW_OP_deref)
  markUsed(gen.callMe())
}

