// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

// Verify that variables bound in the foreach statements are in distinct scopes.
let values = [1, 2, 3]
// CHECK-DAG: ![[SCOPE1:[0-9]+]] ={{.*}}Block(scope: ![[MAIN:[0-9]+]],{{.*}}line: 7,
// CHECK-DAG: ![[SCOPE2:[0-9]+]] ={{.*}}Block(scope: ![[MAIN]], {{.*}}line: 10,
for val in values {
// CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE1]]
}
for val in values {
// CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE2]]
}
