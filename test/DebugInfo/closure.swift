// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

func foldl1<T>(_ list: [T], _ function: (_ a: T, _ b: T) -> T) -> T {
     assert(list.count > 1)
     var accumulator = list[0]
     for i in 1 ..< list.count {
         accumulator = function(accumulator, list[i])
     }
     return accumulator
}

var a = [Int64](repeating: 0, count: 10)
for i in 0..<10 { a[i] = Int64(i) }
// A closure is not an artificial function (the last i32 0).
// CHECK: !DISubprogram({{.*}}linkageName: "$s7closures5Int64VAC_ACtXEfU_",{{.*}} line: 20,{{.*}} scopeLine: 20,
// CHECK: !DILocalVariable(name: "$0", arg: 1{{.*}} line: [[@LINE+2]],
// CHECK: !DILocalVariable(name: "$1", arg: 2{{.*}} line: [[@LINE+1]],
var sum:Int64 = foldl1(a, { $0 + $1 })
markUsed(sum)
