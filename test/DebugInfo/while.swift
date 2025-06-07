// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s

func yieldValues() -> Int64? { return .none }

// Verify that variables bound in the while statements are in distinct scopes.
while let val = yieldValues() {
// CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE1:[0-9]+]]
// CHECK-DAG: ![[SCOPE1]] = distinct !DILexicalBlock(scope: ![[MAIN:[0-9]+]]
}
while let val = yieldValues() {
// CHECK-DAG: !DILocalVariable(name: "val", scope: ![[SCOPE2:[0-9]+]]
// CHECK-DAG: ![[SCOPE2]] = distinct !DILexicalBlock(scope: ![[MAIN2:[0-9]+]]
}
