// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

func curry<T, U, V>(f: (T, U) -> V)(_ x: T)(_ y: U) -> V {
  return f(x, y)
}

func curry<T1, T2, T3, T4>(f: (T1, T2, T3) -> T4)(_ x: T1)(_ y: T2)(_ z: T3) -> T4 {
  return f(x, y, z)
}

func concat(x: String, _ y: String, _ z: String) -> String {
  return x + y + z
}

@inline(never)
public func test_concat_closure(var x: Int) -> String {
  let insult = curry(concat)("one ")(" two ")
  var gs = insult(" three ")
  if (x > 0) {
    gs = gs + insult(" four ")
    gs = gs + insult(" five ")
  } else {
    gs = gs + insult(" six ")
    gs = gs + insult(" seven ")
  }
  if (x > 10) {
     x += 100
  }
  return gs
}

public protocol P {
  func val() -> Int32
}

struct CP: P {
  let v: Int32

  func val() -> Int32 {
     return v
  }

  init(_ v: Int32) {
    self.v = v
  }
}

func compose(x: P, _ y: P, _ z: P) -> Int32 {
  return x.val() + y.val() + z.val()
}


@inline(never)
public func test_compose_closure(var x:Int) -> Int32 {
  let insult = curry(compose)(CP(1))(CP(2))
  var gs = insult(CP(3))
  if (x > 0) {
    gs = gs + insult(CP(4))
    gs = gs + insult(CP(5))
  } else {
    gs = gs + insult(CP(6))
    gs = gs + insult(CP(7))
  }
  if (x > 10) {
     x += 100
  }
  return gs
}

let insult = curry(+)("I'm with stupid ☞ ")
print(insult("😡")) // CHECK: I'm with stupid ☞ 😡

let plus1 = curry(+)(1)
print(plus1(5)) // CHECK-NEXT: 6

let plus5 = curry(+)(5)
print(plus5(5)) // CHECK-NEXT: 10

print(insult("😰")) // CHECK-NEXT: I'm with stupid ☞ 😰


let concat_one_two = curry(concat)("one ")(" two ")
print(concat_one_two(" three ")) // CHECK-NEXT: one two three

print(test_concat_closure(20)) // CHECK-NEXT: one  two  three one  two  four one  two  five

print(test_compose_closure(20)) // CHECK-NEXT: 21

// rdar://problem/18988428

func clamp<T: Comparable>(minValue: T, _ maxValue: T)(n: T) -> T {
    return max(minValue, min(n, maxValue))
}

let clampFoo2 = clamp(10.0, 30.0)

print(clampFoo2(n: 3.0)) // CHECK-NEXT: 10.0

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
print((b <+> pair)(a!)) // CHECK-NEXT: (42, 23)
print((b <+> pair_)(a!)) // CHECK-NEXT: (42, 23)

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


print(view(_1)((1, 2))) // CHECK-NEXT: 1
print(over(_1)({ $0 * 4 })((1, 2))) // CHECK-NEXT: (4, 2)
print(set(_1)(3)((1, 2))) // CHECK-NEXT: (3, 2)

print(view(_2)("hello", 5)) // CHECK-NEXT: 5

