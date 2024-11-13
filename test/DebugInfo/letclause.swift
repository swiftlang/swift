// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Test debug info for storageless variables.
struct Symbol {}
func peek() -> Symbol? { return Symbol() }

func foo() {
// CHECK: define {{.*}}foo
// CHECK: #dbg_value(i{{.*}} 0,
// CHECK-SAME:                      ![[S:.*]], !DIExpression()
// CHECK: ![[S]] = !DILocalVariable(name: "s"
// CHECK-SAME:                      line: [[@LINE+1]],
  while let s = peek() {
    print(s, terminator: "")
  }
}
