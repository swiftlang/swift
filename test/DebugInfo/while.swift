// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

func yieldValues() -> Int64? { return .none }

// Verify that variables bound in the while statements are in distinct scopes.
while let val = yieldValues() {
// CHECK: ![[SCOPE1:[0-9]+]] = distinct !DILexicalBlock(scope: ![[MAIN:[0-9]+]]
// CHECK: !DILocalVariable(name: "val", scope: ![[SCOPE1]]
}
while let val = yieldValues() {
// CHECK: ![[SCOPE2:[0-9]+]] = distinct !DILexicalBlock(scope: ![[MAIN]]
// CHECK: !DILocalVariable(name: "val", scope: ![[SCOPE2]]
}
