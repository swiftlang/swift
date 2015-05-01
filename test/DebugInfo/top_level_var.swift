// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}
var a = 1
markUsed(a)
// Verify that global variables are emitted once in main, once as
// global variable.
// CHECK: ![[MAIN:.*]] = !DISubprogram(name: "main",{{.*}} line: 1
// CHECK: ![[MOD:.*]] = !MDModule(name: "top_level_var"
// CHECK: !DIGlobalVariable(name: "a",
// CHECK-SAME:              scope: ![[MOD]]
// CHECK-SAME:              isDefinition: true
