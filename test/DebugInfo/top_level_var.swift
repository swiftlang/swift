// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

var a = 1
println(a)
// Verify that global variables are emitted once in main, once as
// global variable.
// CHECK: ![[MAIN:.*]] ={{.*}}; [ DW_TAG_subprogram ] [line 1] [def] [scope 0] [main]
// CHECK: ![[MOD:.*]] ={{.*}}; [ DW_TAG_module ] [top_level_var]
// CHECK: ![[MOD]],{{.*}}; [ DW_TAG_variable ] [a] [line 3] [def]
