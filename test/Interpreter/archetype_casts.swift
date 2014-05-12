// RUN: %target-run-simple-swift | FileCheck %s

class C {
  init() {}

  func bar() { println("bar") }
}

class D : C {
  func foo() { println("foo") }
}

func down<T : C>(x: C, _: T.Type) -> T {
  return (x as T)!
}

func up<T : C>(x: T) -> C {
  return x
}

func isa<T : C>(x: C, _: T.Type) -> Bool {
  return x is T
}

// CHECK: foo
down(D(), D.self).foo()
// CHECK: bar
up(D()).bar()
// CHECK: true
println(isa(D(), D.self))
// CHECK: false
println(isa(C(), D.self))
