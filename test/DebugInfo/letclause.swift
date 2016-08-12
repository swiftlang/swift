// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Test debug info for storageless variables.
struct Symbol {}
func peek() -> Symbol? { return Symbol() }

func foo() {
// CHECK: define {{.*}}foo
// CHECK: call void @llvm.dbg.value(metadata i{{.*}} 0,
// CHECK-SAME:                        metadata ![[S:.*]], metadata !{{[0-9]+}})
// CHECK: ![[S]] = !DILocalVariable(name: "s"
// CHECK-SAME:                      line: [[@LINE+1]],
  while let s = peek() {
    print(s, terminator: "")
  }
}
