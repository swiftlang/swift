// RUN: %target-swift-frontend -g -emit-ir %s | FileCheck %s

// Verify that variables bound in the for statements are in distinct scopes.

for i in 0..<3 {
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE1:[0-9]+]]
// CHECK: ![[SCOPE1]] = distinct !DILexicalBlock(scope: ![[MAIN:[0-9]+]]
}
for i in 0..<3 {
// CHECK: !DILocalVariable(name: "i", scope: ![[SCOPE2:[0-9]+]]
// CHECK: ![[SCOPE2]] = distinct !DILexicalBlock(scope: ![[MAIN]]
}
