// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

// Verify that variables bound in the for statements are in distinct scopes.

for i in 0 ..< 3 {
// CHECK-DAG: !DILocalVariable(name: "i", scope: ![[SCOPE1:[0-9]+]]{{.*}}line: [[@LINE-1]]
// CHECK-DAG: ![[SCOPE1]] = {{.*}}DILexicalBlock({{.*}}line: [[@LINE-2]] 
}

for i in 0 ..< 3 {
// CHECK-DAG: !DILocalVariable(name: "i", scope: ![[SCOPE2:[0-9]+]]{{.*}}line: [[@LINE-1]]
// CHECK-DAG: ![[SCOPE2]] = {{.*}}DILexicalBlock({{.*}}line: [[@LINE-2]]
}
