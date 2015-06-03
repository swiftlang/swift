// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

func divide(a: Int, byDividend b: Int) -> Int { return a / b }

print(divide(12, byDividend: 4)) // CHECK: 3
print(divide(12, byDividend:3)) // CHECK: 4

var f : (_:Int, byDividend:Int) -> Int = divide

print(f(20, byDividend:2)) // CHECK: 10

func divide(a: Int, byDividends b: Int, _ c: Int, thenAdd d: Int) -> Int {
    return a / b / c + d
}

func divide(a: Int, byDividends b: Int, _ c: Int, _ d: Int, thenAdd e: Int) -> Int {
    return a / b / c / d + e
}

print(divide(60, byDividends:2, 3, thenAdd:100)) // CHECK: 110
print(divide(60, byDividends:2, 3, 5, thenAdd:100)) // CHECK: 102

var g : (_:Int, byDividends:Int, _:Int, thenAdd:Int) -> Int = divide
var h : (_:Int, byDividends:Int, _:Int, _:Int, thenAdd:Int) -> Int = divide

print(g(60, byDividends:2, 3, thenAdd:300)) // CHECK: 310
print(h(60, byDividends:2, 3, 5, thenAdd:300)) // CHECK: 302
