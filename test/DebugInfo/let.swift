// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

class DeepThought {
  func query() -> Int64 { return 42 }
}

func foo() -> Int64 {
  // CHECK: #dbg_declare(ptr {{.*}}, ![[A:.*]], !DIExpression()
  // CHECK-DAG: !DILocalVariable(name: "machine",{{.*}}line: [[@LINE+1]], type: !{{[0-9]+}})
  let machine = DeepThought()
  // CHECK-DAG: !DILocalVariable(name: "a", {{.*}}line: [[@LINE+1]],
  let a = machine.query()
  return a
}
