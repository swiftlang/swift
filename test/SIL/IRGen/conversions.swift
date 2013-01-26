// RUN: %swift -sil-i %s | FileCheck %s

class B { func foo() { println("foo") } }
class D : B { func bar() { println("bar") } }

// CHECK: foo
func up(d:D) { d.foo() }
// CHECK: bar
func down(b:B) { D(b).bar() }

up(new D)
down(new D)
