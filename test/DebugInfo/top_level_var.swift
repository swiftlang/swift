// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}
var a = 1
markUsed(a)
// Verify that global variables are emitted once in main, once as
// global variable.
// CHECK: !DIGlobalVariable(name: "a",
// CHECK-SAME:              scope: ![[MOD:[0-9]+]]
// CHECK-SAME:              isDefinition: true
// CHECK: ![[MOD]] = !DIModule({{.*}}, name: "top_level_var"
// CHECK: ![[MAIN:.*]] = distinct !DISubprogram(name: "main",{{.*}} line: 1
