// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

func foo(_ a : Int64, _ b : Int64) -> Int64 {
  return a+b
}

// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG1:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG2:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[OUTER:.*]]
let r = foo(foo(1, 23), // CHECK: ![[ARG1]] = !DILocation(line: [[@LINE]],
            foo(2, 42)  // CHECK: ![[ARG2]] = !DILocation(line: [[@LINE]],
           )            // CHECK: ![[OUTER]] = !DILocation(line: [[@LINE]],
markUsed(r)

