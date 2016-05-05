// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

protocol A {}
protocol B {}
typealias C = protocol<B, A>
protocol D {}
var p: protocol<C, D>?
// CHECK: !DIGlobalVariable(name: "p", {{.*}}, type: ![[PTY:[0-9]+]]
// CHECK: ![[PTY]] = !DICompositeType(tag: DW_TAG_union_type, name: "Optional", {{.*}}, identifier: "_TtGSqP4main1AS_1BS_1D__")

