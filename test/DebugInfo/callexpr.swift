// RUN: %target-swift-frontend %s -g -emit-ir -o - | FileCheck %s

func foo(a : Int, _ b : Int) -> Int {
  return a+b
}

// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG1:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG2:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[OUTER:.*]]
let r = foo(foo(1, 23), // CHECK: ![[ARG1]] = !MDLocation(line: [[@LINE]],
            foo(2, 42)  // CHECK: ![[ARG2]] = !MDLocation(line: [[@LINE]],
           )            // CHECK: ![[OUTER]] = !MDLocation(line: [[@LINE]],
println(r)

