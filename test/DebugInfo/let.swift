// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

class DeepThought {
  func query() -> Int64 { return 42 }
}

func foo() -> Int64 {
  // CHECK: call void @llvm.dbg.declare(metadata %T3let11DeepThoughtC** {{.*}}, metadata ![[A:.*]], metadata !{{[0-9]+}})
  // CHECK: !DILocalVariable(name: "machine"
  // CHECK-NOT:              flags:
  // CHECK-SAME:             line: [[@LINE+1]],
  let machine = DeepThought()
  // CHECK: !DILocalVariable(name: "a"
  // CHECK-SAME:             line: [[@LINE+1]],
  let a = machine.query()
  return a
}
