// RUN: %swift -target x86_64-apple-darwin13 %s -emit-ir -g -o - | FileCheck %s
class DeepThought {
  func query() -> Int { return 42 }
}

func foo() -> Int {
  // CHECK: call void @llvm.dbg.value(metadata !{%C3let11DeepThought* {{.*}}}, i64 0, metadata ![[A:.*]])
  // CHECK ![[A]] = {{.*}}i32 0} ; [ DW_TAG_auto_variable ] [machine] [line [[@LINE+1]]]
  let machine = DeepThought()
// CHECK: [ DW_TAG_auto_variable ] [a] [line [[@LINE+1]]]
  let a = machine.query()
  return a
}
