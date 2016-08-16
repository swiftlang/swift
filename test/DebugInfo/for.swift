// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

// Verify that variables bound in the for statements are in distinct scopes.

for i in 0 ..< 3 {
// CHECK: ![[SCOPE1:[0-9]+]] = {{.*}}Block(scope: ![[MAIN:[0-9]+]],{{.*}}line: 5
}

for i in 0 ..< 3 {
// CHECK: ![[SCOPE2:[0-9]+]] = {{.*}}Block(scope: ![[MAIN]],{{.*}}line: 9
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE1]]
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE2]]
}
