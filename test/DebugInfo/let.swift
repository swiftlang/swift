// RUN: %target-swift-frontend %use_no_opaque_pointers -primary-file %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o -

class DeepThought {
  func query() -> Int64 { return 42 }
}

func foo() -> Int64 {
  // CHECK: call void @llvm.dbg.declare(metadata %T3let11DeepThoughtC** {{.*}}, metadata ![[A:.*]], metadata !DIExpression())
  // CHECK-DAG: !DILocalVariable(name: "machine",{{.*}}line: [[@LINE+1]], type: !{{[0-9]+}})
  let machine = DeepThought()
  // CHECK-DAG: !DILocalVariable(name: "a", {{.*}}line: [[@LINE+1]],
  let a = machine.query()
  return a
}
