// RUN: %swift -i %s | FileCheck %s

class B { func foo() { println("foo") } }
class D : B { func bar() { println("bar") } }
class G<T> : B { func bas() { println("bas") } }

// CHECK: foo
func up(d:D) { d.foo() }
// CHECK: bar
func down(b:B) { (b as! D).bar() }
// CHECK: bas
func down_generic(b:B) { (b as! G<Int>).bas() }

up(D())
down(D())
down_generic(G<Int>())
