// RUN: %target-swift-frontend -g -emit-ir %s | FileCheck %s

// Verify that variables bound in the for statements are in distinct scopes.

for var i = 0; i < 3; i += 1 {
// CHECK: ![[SCOPE1:[0-9]+]] = {{.*}}Block(scope: ![[MAIN:[0-9]+]],{{.*}}line: 5
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE1]]
}
for var i = 0; i < 3; i += 1 {
// CHECK: ![[SCOPE2:[0-9]+]] = {{.*}}Block(scope: ![[MAIN:[0-9]+]],{{.*}}line: 9
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE2]]
}
