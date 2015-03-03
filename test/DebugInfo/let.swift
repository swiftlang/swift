// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

class DeepThought {
  func query() -> Int { return 42 }
}

func foo() -> Int {
  // CHECK: call void @llvm.dbg.declare(metadata %C3let11DeepThought** {{.*}}, metadata ![[A:.*]], metadata !{{[0-9]+}})
  // CHECK ![[A]] = {{.*}}i32 0} ; [ DW_TAG_auto_variable ] [machine] [line [[@LINE+1]]]
  // CHECK: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "machine"
  // CHECK-NOT:              flags:
  // CHECK-SAME:             line: [[@LINE+1]],
  let machine = DeepThought()
  // CHECK: !MDLocalVariable(tag: DW_TAG_auto_variable, name: "a"
  // CHECK-SAME:             line: [[@LINE+1]],
  let a = machine.query()
  return a
}
