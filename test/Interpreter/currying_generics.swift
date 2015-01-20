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

// rdar://problem/18988428

func clamp<T: Comparable>(minValue: T, maxValue: T)(n: T) -> T {
    return max(minValue, min(n, maxValue))
}

let clampFoo2 = clamp(10.0, 30.0)

println(clampFoo2(n: 3.0)) // CHECK-NEXT: 10.0

// rdar://problem/19195470

func pair<T,U> (a: T) -> U -> (T,U) {
	return { b in (a,b)	}
}

func pair_<T,U> (a: T)(b: U) -> (T,U) {
	return (a,b)
}

infix operator <+> { }
func <+><T,U,V> (lhs: T?, rhs: T -> U -> V) -> U -> V? {
	if let x = lhs {
		return rhs(x)
	} else {
		return { _ in nil }
	}
}

let a : Int? = 23
let b : Int? = 42
println((b <+> pair)(a!)) // CHECK-NEXT: (42, 23)
println((b <+> pair_)(a!)) // CHECK-NEXT: (42, 23)
