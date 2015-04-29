// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func foldl1<T>(list: [T], _ function: (a: T, b: T) -> T) -> T {
     assert(list.count > 1)
     var accumulator = list[0]
     for var i = 1; i < list.count; ++i {
         accumulator = function(a: accumulator, b: list[i])
     }
     return accumulator
}

var a = [Int](count: 10, repeatedValue: 0)
for i in 0..<10 { a[i] = i }
// A closure is not an artificial function (the last i32 0).
// CHECK: !DISubprogram({{.*}}linkageName: "_TF7closureU_FTSiSi_Si",{{.*}} line: 18,{{.*}} scopeLine: 18,
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "$0",{{.*}} line: [[@LINE+2]],
// CHECK: !DILocalVariable(tag: DW_TAG_arg_variable, name: "$1",{{.*}} line: [[@LINE+1]],
var sum:Int = foldl1(a, { $0 + $1 })
println(sum)
