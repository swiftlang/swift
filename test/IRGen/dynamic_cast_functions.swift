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
