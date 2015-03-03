// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

var a = 1
println(a)
// Verify that global variables are emitted once in main, once as
// global variable.
// CHECK: ![[MAIN:.*]] = !MDSubprogram(name: "main",{{.*}} line: 1
// CHECK: ![[MOD:.*]] = !MDModule(name: "top_level_var"
// CHECK: !MDGlobalVariable(name: "a",
// CHECK-SAME:              scope: ![[MOD]]
// CHECK-SAME:              isDefinition: true
