// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

func foldl1<T>(list: [T], _ function: (a: T, b: T) -> T) -> T {
     assert(list.count > 1)
     var accumulator = list[0]
     for var i = 1; i < list.count; i += 1 {
         accumulator = function(a: accumulator, b: list[i])
     }
     return accumulator
}

var a = [Int64](count: 10, repeatedValue: 0)
for i in 0..<10 { a[i] = Int64(i) }
// A closure is not an artificial function (the last i32 0).
// CHECK: !DISubprogram({{.*}}linkageName: "_TF7closureU_FTVs5Int64S0__S0_",{{.*}} line: 20,{{.*}} scopeLine: 20,
// CHECK: !DILocalVariable(name: "$0", arg: 1{{.*}} line: [[@LINE+2]],
// CHECK: !DILocalVariable(name: "$1", arg: 2{{.*}} line: [[@LINE+1]],
var sum:Int64 = foldl1(a, { $0 + $1 })
markUsed(sum)
