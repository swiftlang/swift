// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}
func use<T>(_ t: inout T) {}

public protocol IGiveOutInts {
  func callMe() -> Int64
}

// CHECK: define {{.*}}@_T011protocolarg16printSomeNumbersyAA12IGiveOutInts_pF
// CHECK: @llvm.dbg.declare(metadata %T11protocolarg12IGiveOutIntsP** %
// CHECK-SAME:              metadata ![[ARG:.*]], metadata ![[DEREF:.*]])
// CHECK: @llvm.dbg.declare(metadata %T11protocolarg12IGiveOutIntsP* %
// CHECK-SAME:              metadata ![[VAR:.*]], metadata ![[EMPTY:.*]])

// CHECK: ![[EMPTY]] = !DIExpression()

public func printSomeNumbers(_ gen: IGiveOutInts) {
  var gen = gen
  // FIXME: Should be DW_TAG_interface_type
  // CHECK: ![[ARG]] = !DILocalVariable(name: "gen", arg: 1,
  // CHECK-SAME:                        line: [[@LINE-4]], type: ![[PT:[0-9]+]]
  // CHECK: ![[PT]] = !DICompositeType(tag: DW_TAG_structure_type, name: "IGiveOutInts"
  // CHECK: ![[DEREF]] = !DIExpression(DW_OP_deref)
  // CHECK: ![[VAR]] = !DILocalVariable(name: "gen", {{.*}} line: [[@LINE-6]]
  markUsed(gen.callMe())
  use(&gen)
}

