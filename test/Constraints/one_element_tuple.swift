// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

let t1: (_: Int) = (_: 3)
let t2: (x: Int) = (x: 3)

let i1: Int = t1.0
let i2: Int = t2.x

let m1: (_: Int).Type = (_: Int).self
let m2: (x: Int).Type = (x: Int).self
