// RUN: %swift -triple x86_64-apple-darwin13 %s -emit-llvm -g -o - | FileCheck %s
class DeepThought {
  func query() -> Int { return 42 }
}

func foo() -> Int {
  let machine = DeepThought()
// CHECK: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
  let a = machine.query()
  return a
}
