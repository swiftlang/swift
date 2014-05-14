// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir -g -o - | FileCheck %s

func foldl1<T>(list: T[], function: (a: T, b: T) -> T) -> T {
     assert(list.count > 1)
     var accumulator = list[0]
     for var i = 1; i < list.count; ++i {
         accumulator = function(a: accumulator, b: list[i])
     }
     return accumulator
}

var a = new Int[10]
for i in 0..10 { a[i] = i }
// A closure is not an artificial function (the last i32 0).
// CHECK: metadata !"_TF7closureU_FTSiSi_Si", i32 [[@LINE+3]], metadata !{{[0-9]+}}, i1 false, i1 true, i32 0, i32 0, null, i32 0, i1 false, {{.*}}[ DW_TAG_subprogram ]
// CHECK: [ DW_TAG_arg_variable ] [$0] [line [[@LINE+2]]]
// CHECK: [ DW_TAG_arg_variable ] [$1] [line [[@LINE+1]]]
var sum:Int = foldl1(a, { $0 + $1 })
println(sum)
