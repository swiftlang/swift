// RUN: %swift -primary-file %s -emit-ir -g -o - | FileCheck %s
// XFAIL: linux
// Test debug info for storageless variables.
struct Symbol {}
func peek() -> Symbol? { return Symbol() }

func foo() {
// CHECK: define {{.*}}foo
// CHECK: call void @llvm.dbg.declare(metadata ![[UNDEF:.*]], metadata ![[S:.*]], metadata !{{[0-9]+}})
// CHECK: ![[UNDEF]] = metadata !{%V9letclause6Symbol undef}
// CHECK: ![[S]] = {{.*}}[ DW_TAG_auto_variable ] [s] [line [[@LINE+1]]]
  while let s = peek() {
    print(s)
  }
}
