// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s
protocol A {}
protocol B {}
typealias C = B & A
protocol D {}
var p: (C & D)?
// CHECK-DAG: !DIGlobalVariable(name: "p", {{.*}}type: ![[TY:[0-9]+]]
// CHECK-DAG: ![[TY]] = {{.*}}identifier: "_T04main1A_AA1BAA1DpSgD"
