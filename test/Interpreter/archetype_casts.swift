// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

class C {
  init() {}

  func bar() { print("bar") }
}

class D : C {
  func foo() { print("foo") }
}

func down<T : C>(x: C, _: T.Type) -> T {
  return x as! T
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
print(isa(D(), D.self))
// CHECK: false
print(isa(C(), D.self))
