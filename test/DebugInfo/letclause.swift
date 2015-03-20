// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s
// XFAIL: *

// Test debug info for storageless variables.
struct Symbol {}
func peek() -> Symbol? { return Symbol() }

func foo() {
// CHECK: define {{.*}}foo
// CHECK: call void @llvm.dbg.declare(metadata %V9letclause6Symbol undef, metadata ![[S:.*]], metadata !{{[0-9]+}})
// CHECK: ![[S]] = !MDLocalVariable(tag: DW_TAG_auto_variable, name: "s"
// CHECK-SAME:                      line: [[@LINE+1]],
  while let s = peek() {
    print(s)
  }
}
