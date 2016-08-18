// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

func curry<T, U, V>(_ f: @escaping (T, U) -> V) -> @escaping (T) -> @escaping (U) -> V {
  return { x in { y in f(x, y) } }
}

func curry<T1, T2, T3, T4>(_ f: @escaping (T1, T2, T3) -> T4) -> @escaping (T1) -> @escaping (T2) -> @escaping (T3) -> T4 {
  return { x in { y in { z in f(x, y, z) } } }
}

func concat(_ x: String, _ y: String, _ z: String) -> String {
  return x + y + z
}

@inline(never)
public func test_concat_closure(_ x: Int) -> String {
  var x = x
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

func compose(_ x: P, _ y: P, _ z: P) -> Int32 {
  return x.val() + y.val() + z.val()
}


@inline(never)
public func test_compose_closure(_ x: Int) -> Int32 {
  var x = x
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

func clamp<T: Comparable>(_ minValue: T, _ maxValue: T) -> @escaping (_ n: T) -> T {
    return { n in max(minValue, min(n, maxValue)) }
}

let clampFoo2 = clamp(10.0, 30.0)

print(clampFoo2(3.0)) // CHECK-NEXT: 10.0

// rdar://problem/19195470

func pair<T,U> (_ a: T) -> (U) -> (T,U) {
	return { b in (a,b)	}
}

func pair_<T,U> (_ a: T) -> (_ b: U) -> (T,U) {
	return { b in (a,b) }
}

infix operator <+> { }
func <+><T,U,V> (lhs: T?, rhs: @escaping (T) -> @escaping (U) -> V) -> @escaping (U) -> V? {
	if let x = lhs {
		return { y in .some(rhs(x)(y)) }
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

func fmap<A, B>(_ f: @escaping (A) -> B) -> @escaping (Identity<A>) -> Identity<B> {
	return { identity in Identity(value: f(identity.value)) }
}

func fmap<A, B>(_ f: @escaping (A) -> B) -> @escaping (Const<A, B>) -> Const<A, B> {
	return { const in const }
}

// really Const()
func _Const<A, B>(_ a: A) -> Const<A, B> {
	return Const(value: a)
}
func const<A, B>(_ a: A) -> (B) -> A {
	return { _ in a }
}

// really Identity()
func _Identity<A>(_ a: A) -> Identity<A> {
	return Identity(value: a)
}

func getConst<A, B>(_ c: Const<A, B>) -> A {
	return c.value
}

func runIdentity<A>(_ i: Identity<A>) -> A {
	return i.value
}


func view<S, A>(_ lens: @escaping (@escaping (A) -> Const<A, S>) -> @escaping (S) -> @escaping (@escaping (@escaping (A) -> S) -> @escaping (Const<A, S>) -> Const<A, S>) -> Const<A, S>) -> @escaping (S) -> A {
	return { s in getConst(lens(_Const)(s)(fmap)) }
}

func over<S, A>(_ lens: @escaping (@escaping (A) -> Identity<A>) -> @escaping (S) -> @escaping (@escaping (@escaping (A) -> S) -> @escaping (Identity<A>) -> Identity<S>) -> Identity<S>) -> @escaping (@escaping (A) -> A) -> @escaping (S) -> S {
	return { f in { s in runIdentity(lens({ _Identity(f($0)) })(s)(fmap)) } }
}

func set<S, A>(_ lens: @escaping (@escaping (A) -> Identity<A>) -> @escaping (S) -> @escaping (@escaping (@escaping (A) -> S) -> @escaping (Identity<A>) -> Identity<S>) -> Identity<S>) -> @escaping (A) -> @escaping (S) -> S {
	return { x in { y in over(lens)(const(x))(y) } }
}

func _1<A, B, C, D>(_ f: @escaping (A) -> C) -> @escaping (A, B) -> @escaping (@escaping (@escaping (A) -> (A, B)) -> @escaping (C) -> D) -> D {
	return { (x, y) in { fmap in fmap({ ($0, y) })(f(x)) } }
}

func _2<A, B, C, D>(_ f: @escaping (B) -> C) -> @escaping (A, B) -> @escaping (@escaping (@escaping (B) -> (A, B)) -> @escaping (C) -> D) -> D {
    return { (x, y) in { fmap in fmap({ (x, $0) })(f(y)) } }
}


public func >>> <T, U, V> (f: @escaping (T) -> U, g: @escaping (U) -> V) -> @escaping (T) -> V {
	return { g(f($0)) }
}

public func <<< <T, U, V> (f: @escaping (U) -> V, g: @escaping (T) -> U) -> @escaping (T) -> V {
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

let pt1 = view(_1)((1, 2))
print(pt1) // CHECK-NEXT: 1
let pt2 = over(_1)({ $0 * 4 })((1, 2))
print(pt2) // CHECK-NEXT: (4, 2)
let pt3 = set(_1)(3)((1, 2))
print(pt3) // CHECK-NEXT: (3, 2)
let pt4 = view(_2)("hello", 5)
print(pt4) // CHECK-NEXT: 5

