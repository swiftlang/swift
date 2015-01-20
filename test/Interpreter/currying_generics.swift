// RUN: %target-run-simple-swift | FileCheck %s

func curry<T, U, V>(f: (T, U) -> V)(_ x: T)(_ y: U) -> V {
  return f(x, y)
}

let insult = curry(+)("I'm with stupid ☞ ")
println(insult("😡")) // CHECK: I'm with stupid ☞ 😡

let plus1 = curry(+)(1)
println(plus1(5)) // CHECK-NEXT: 6

let plus5 = curry(+)(5)
println(plus5(5)) // CHECK-NEXT: 10

println(insult("😰")) // CHECK-NEXT: I'm with stupid ☞ 😰
