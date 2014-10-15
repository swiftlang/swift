// RUN: %target-build-swift %s -g -emit-ir -o - | FileCheck %s
func foo(a : Int, b : Int) -> Int {
  return a+b
}

// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG1:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG2:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[OUTER:.*]]
let r = foo(foo(1, 23), // CHECK: ![[ARG1]] = metadata !{i32 [[@LINE]],
            foo(2, 42)  // CHECK: ![[ARG2]] = metadata !{i32 [[@LINE]],
           )            // CHECK: ![[OUTER]] = metadata !{i32 [[@LINE]],
println(r)

