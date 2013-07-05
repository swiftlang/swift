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

func isa<T:C>(x:C, _:T.metatype) -> Bool {
  return x is T
}

// CHECK: foo
down(D(), D).foo()
// CHECK: bar
up(D()).bar()
// CHECK: true
println(isa(D(), D))
// CHECK: false
println(isa(C(), D))
