// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

class B {     func foo() { print("foo") } }
class D : B {    func bar() { print("bar") } }
class G<T> : B {   func bas() { print("bas") } }

// CHECK: foo
func up(_ d: D) { d.foo() }
// CHECK: bar
func down(_ b: B) { (b as! D).bar() }
// CHECK: bas
func down_generic(_ b: B) { (b as! G<Int>).bas() }

up(D())
down(D())
down_generic(G<Int>())
