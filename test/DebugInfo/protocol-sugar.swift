// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
protocol A {}
protocol B {}
typealias C = protocol<B, A>
protocol D {}
var p: protocol<C, D>?
// CHECK-DAG: !DIGlobalVariable(name: "p", {{.*}}type: ![[TY:[0-9]+]]
// CHECK-DAG: ![[TY]] = {{.*}}identifier: "_TtGSqP4main1AS_1BS_1D__"
