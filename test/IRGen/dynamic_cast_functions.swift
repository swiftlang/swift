// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

let a: ((Int, Int)) -> Void = { x in }
let b: Any = a

let cast1 = b as? (Int, Int) -> Void
let cast2 = b as? ((Int, Int)) -> Void

// CHECK: ok
print((cast1 == nil) ? "ok" : "fail")
// CHECK: ok
print((cast2 != nil) ? "ok" : "fail")

let c: () -> Void = { }
let d: (()) -> Void = { x in }

let e: Any = c
let f: Any = d

let cast3 = e as? () -> Void
let cast4 = e as? (()) -> Void
let cast5 = f as? () -> Void
let cast6 = f as? (()) -> Void

// CHECK: ok
print((cast3 != nil) ? "ok" : "fail")
// CHECK: ok
print((cast4 == nil) ? "ok" : "fail")
// CHECK: ok
print((cast5 == nil) ? "ok" : "fail")
// CHECK: ok
print((cast6 != nil) ? "ok" : "fail")

let g: (Int, Int) -> Void = { x, y in }
let h: Any = g

let cast7 = h as? ((Int, Int)) -> Void
let cast8 = h as? (Int, Int) -> Void

// CHECK: ok
print((cast7 == nil) ? "ok" : "fail")
// CHECK: ok
print((cast8 != nil) ? "ok" : "fail")

let t1: Any.Type = ((Int, Int) -> ()).self
let t2: Any.Type = (((Int, Int)) -> ()).self

// CHECK: ok
print((t1 == t2) ? "fail" : "ok")

let i: (inout Int) -> Void = { _ in }
let j: (__shared Int) -> Void = { _ in }
let k: (Int, inout Int) -> Void = { _,_ in }
let l: (inout Int, Float, inout String) -> Void = { _,_,_ in }
let m: (__shared Int, String, inout Float, Double) -> Void = { _,_,_,_ in }
let n: () -> Int = { 42 }
let o: (@autoclosure () -> Int) -> Void = { (x: @autoclosure () -> Int) -> Void in }
let p: (@autoclosure @escaping () -> Int) -> Void = { (x: @autoclosure @escaping () -> Int) -> Void in }

let i_any: Any = i
let j_any: Any = j
let k_any: Any = k
let l_any: Any = l
let m_any: Any = m
let n_any: Any = n
let o_any: Any = o
let p_any: Any = p

// CHECK: ok
print((i_any as? (Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((i_any as? (__shared Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((i_any as? (inout Int) -> Void) != nil ? "ok" : "fail")

// CHECK: ok
print((j_any as? (Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((j_any as? (inout Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((j_any as? (__shared Int) -> Void) != nil ? "ok" : "fail")

// CHECK: ok
print((k_any as? (Int, Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((k_any as? (Int, inout Int) -> Void) != nil ? "ok" : "fail")
// CHECK: ok
print((k_any as? (inout Int, Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((k_any as? (inout Int, __shared Int) -> Void) != nil ? "fail" : "ok")

// CHECK: ok
print((l_any as? (Int, Float, String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (Int, Float, inout String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (Int, inout Float, String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (inout Int, Float, String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (inout Int, inout Float, String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (inout Int, Float, __shared String) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((l_any as? (inout Int, Float, inout String) -> Void) != nil ? "ok" : "fail")

// CHECK: ok
print((m_any as? (Int, String, Float, Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (Int, String, Float, inout Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (Int, String, Float, __shared Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (Int, String, inout Float, Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (Int, __shared String, Float, Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (inout Int, String, __shared Float, Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (__shared Int, String, Float, inout Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (Int, __shared String, inout Float, Double) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((m_any as? (__shared Int, String, inout Float, Double) -> Void) != nil ? "ok" : "fail")
// CHECK: ok
print((n_any as? () -> Int) != nil ? "ok" : "fail")
// CHECK: ok
print((n_any as? () -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((n_any as? (Int) -> Int) != nil ? "fail" : "ok")

// CHECK: ok
print((o_any as? (() -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((o_any as? (inout () -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((o_any as? (@escaping () -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((o_any as? (@autoclosure () -> Int) -> Void) != nil ? "ok" : "fail")
// CHECK: ok
print((o_any as? (@autoclosure @escaping () -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((p_any as? (@escaping () -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((p_any as? (@autoclosure () -> Int) -> Void) != nil ? "fail" : "ok")
// CHECK: ok
print((p_any as? (@autoclosure @escaping () -> Int) -> Void) == nil ? "fail" : "ok")
