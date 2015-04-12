// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

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
		return { y in .Some(rhs(x)(y)) }
	} else {
		return { _ in nil }
	}
}

let a : Int? = 23
let b : Int? = 42
println((b <+> pair)(a!)) // CHECK-NEXT: (42, 23)
println((b <+> pair_)(a!)) // CHECK-NEXT: (42, 23)

//
// rdar://problem/20475584
//

struct Identity<A> { let value: A }
struct Const<A, B> { let value: A }

func fmap<A, B>(f: A -> B)(_ identity: Identity<A>) -> Identity<B> {
	return Identity(value: f(identity.value))
}

func fmap<A, B>(f: A -> B)(_ const: Const<A, B>) -> Const<A, B> {
	return const
}

// really Const()
func _Const<A, B>(a: A) -> Const<A, B> {
	return Const(value: a)
}
func const<A, B>(a: A)(_: B) -> A {
	return a
}

// really Identity()
func _Identity<A>(a: A) -> Identity<A> {
	return Identity(value: a)
}

func getConst<A, B>(c: Const<A, B>) -> A {
	return c.value
}

func runIdentity<A>(i: Identity<A>) -> A {
	return i.value
}


func view<S, A>(lens: (A -> Const<A, S>) -> S -> ((A -> S) -> Const<A, S> -> Const<A, S>) -> Const<A, S>)(_ s: S) -> A {
	return getConst(lens(_Const)(s)(fmap))
}

func over<S, A>(lens: (A -> Identity<A>) -> S -> ((A -> S) -> Identity<A> -> Identity<S>) -> Identity<S>)(_ f: A -> A)(_ s: S) -> S {
	return runIdentity(lens({ _Identity(f($0)) })(s)(fmap))
}

func set<S, A>(lens: (A -> Identity<A>) -> S -> ((A -> S) -> Identity<A> -> Identity<S>) -> Identity<S>)(_ x: A)(_ y: S) -> S {
	return over(lens)(const(x))(y)
}

func _1<A, B, C, D>(f: A -> C)(_ x: A, _ y: B)(_ fmap: (A -> (A, B)) -> C -> D) -> D {
	return fmap({ ($0, y) })(f(x))
}

func _2<A, B, C, D>(f: B -> C)(_ x: A, _ y: B)(_ fmap: (B -> (A, B)) -> C -> D) -> D {
	return fmap({ (x, $0) })(f(y))
}


public func >>> <T, U, V> (f: T -> U, g: U -> V) -> T -> V {
	return { g(f($0)) }
}

public func <<< <T, U, V> (f: U -> V, g: T -> U) -> T -> V {
	return { f(g($0)) }
}


infix operator >>> {
	associativity right
	precedence 170
}
infix operator <<< {
	associativity right
	precedence 170
}


println(view(_1)((1, 2))) // CHECK-NEXT: 1
println(over(_1)({ $0 * 4 })((1, 2))) // CHECK-NEXT: (4, 2)
println(set(_1)(3)((1, 2))) // CHECK-NEXT: (3, 2)

println(view(_2)("hello", 5)) // CHECK-NEXT: 5

