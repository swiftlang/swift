// RUN: %target-run-simple-swift | FileCheck %s

// Check that Optionals are bitcastable to their payload types. If this ever
// isn't the case in the future, we want to know.
let s: String? = "foo"
println(reinterpretCast(s) as String) // CHECK: foo

let i: Int? = 219
println(reinterpretCast(i) as Int) // CHECK: 219

class C {
  func bar() { println("bar") }
}

let c: C? = C()
(reinterpretCast(c) as C).bar() // CHECK: bar
