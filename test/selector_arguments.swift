// RUN: %swift %s -i | FileCheck %s

func divide(a:Int) byDividend(b:Int) -> Int { return a / b }

println(divide(12, 4)) // CHECK: 3
println(divide(12, byDividend=3)) // CHECK: 4

var f : (_:Int, byDividend:Int) -> Int = divide

println(f(20, 4)) // CHECK: 5
println(f(20, byDividend=2)) // CHECK: 10

func divide(a:Int) byDividends(b:Int) _(c:Int) thenAdd(d:Int) -> Int {
    return a / b / c + d
}

func divide(a:Int) byDividends(b:Int) _(c:Int) _(d:Int) thenAdd(e:Int) -> Int {
    return a / b / c / d + e
}

println(divide(60, byDividends=2, 3, thenAdd=100)) // CHECK: 110
println(divide(120, 2, 3, 200)) // CHECK: 220
println(divide(60, byDividends=2, 3, 5, thenAdd=100)) // CHECK: 102
println(divide(120, 2, 3, 5, 200)) // CHECK: 204

var g : (_:Int, byDividends:Int, _:Int, thenAdd:Int) -> Int = divide
var h : (_:Int, byDividends:Int, _:Int, _:Int, thenAdd:Int) -> Int = divide

println(g(60, byDividends=2, 3, thenAdd=300)) // CHECK: 310
println(g(120, 2, 3, 400)) // CHECK: 420
println(h(60, byDividends=2, 3, 5, thenAdd=300)) // CHECK: 302
println(h(120, 2, 3, 5, 400)) // CHECK: 404

func divide(a:Int) byDividendsInTuple((b,c):(Int, Int)) -> (Int, Int) {
    return (a/b, a/c)
}

var (five, three) = divide(15, byDividendsInTuple=(3,5))
println(five) // CHECK: 5
println(three) // CHECK: 3
