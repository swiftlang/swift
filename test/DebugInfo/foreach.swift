// RUN: %target-swift-frontend -g -emit-ir %s | FileCheck %s

// Verify that variables bound in the foreach statements are in distinct scopes.
let values = [1, 2, 3]
for val in values {
// CHECK: ![[SCOPE1:[0-9]+]] = distinct !DILexicalBlock(scope: ![[MAIN:[0-9]+]]
// CHECK: !DILocalVariable(name: "val", scope: ![[SCOPE1]]
}
for val in values {
// CHECK: ![[SCOPE2:[0-9]+]] = distinct !DILexicalBlock(scope: ![[MAIN]]
// CHECK: !DILocalVariable(name: "val", scope: ![[SCOPE2]]
}
