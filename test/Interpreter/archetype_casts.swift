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

func typeof<T>(x: T) -> T.Type {
  return T.self
}

protocol Scrutinizable {}

extension C : Scrutinizable {}

extension Int : Scrutinizable {}

extension Scrutinizable {
  func scrutinize() -> Any.Type {
    return typeof(self)
  }
}

// CHECK: foo
down(D(), D.self).foo()
// CHECK: bar
up(D()).bar()
// CHECK: true
print(isa(D(), D.self))
// CHECK: false
print(isa(C(), D.self))
// CHECK: C
print(C().scrutinize())
// CHECK: D
print(D().scrutinize())
// CHECK: Int
print(3.scrutinize())
