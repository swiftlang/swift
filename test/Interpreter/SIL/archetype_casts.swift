// RUN: %swift -i %s | FileCheck %s

class C {
  func bar() { println("bar") }
}

class D : C {
  func foo() { println("foo") }
}

func down<T:C>(x:C, _:T.metatype) -> T {
  return x as! T
}

func up<T:C>(x:T) -> C {
  return x
}

// CHECK: foo
down(new D, D).foo()
// CHECK: bar
up(new D).bar()
