// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

// Verify that variables bound in the foreach statements are in distinct scopes.
let values = [1, 2, 3]
for val in values {
  // CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE1:[0-9]+]],{{.*}}line: [[@LINE-1]],
// CHECK-DAG: ![[SCOPE1]] ={{.*}}Block({{.*}}line: [[@LINE-2]]
}
for val in values {
// CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE2:[0-9]+]]
// CHECK-DAG: ![[SCOPE2]] ={{.*}}Block({{.*}}line: [[@LINE-2]]
}
