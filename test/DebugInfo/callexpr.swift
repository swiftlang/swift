// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck --check-prefix=CHECK2 %s

func markUsed<T>(_ t: T) {}

func foo(_ a : Int64, _ b : Int64) -> Int64 {
  return a+b
}

// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG1:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[ARG2:.*]]
// CHECK: call {{.*}}foo{{.*}}, !dbg ![[OUTER:.*]]
let r = foo(
            foo(1, 23), // CHECK: ![[ARG1]] = !DILocation(line: [[@LINE]],
            foo(2, 42)  // CHECK: ![[ARG2]] = !DILocation(line: [[@LINE]],
           )            // CHECK: ![[OUTER]] = !DILocation(line: [[@LINE-3]],
markUsed(r)

struct MyType {}
func bar(x: MyType = MyType()) {}

// CHECK2: call {{.*}}MyType{{.*}}, !dbg ![[DEFAULTARG:.*]]
// CHECK2: call {{.*}}bar{{.*}}, !dbg ![[BARCALL:.*]]
bar() // CHECK2: ![[DEFAULTARG]] = !DILocation(line: 0
      // CHECK2: ![[BARCALL]] = !DILocation(line: [[@LINE-1]], column: 1

