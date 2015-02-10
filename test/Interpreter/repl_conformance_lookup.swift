// RUN: %target-repl-run-simple-swift | FileCheck %s

// REQUIRES: swift_repl

protocol Fooable {
  func foo()
}

class C {}
class D: C {}

func fooify<T>(x: T) {
  if let foo = x as? Fooable {
    foo.foo()
  } else {
    println("--not fooable--")
  }
}

fooify(1) // CHECK: --not fooable--
fooify(1) // CHECK: --not fooable--
fooify(C()) // CHECK: --not fooable--
fooify(C()) // CHECK: --not fooable--
fooify(D()) // CHECK: --not fooable--
fooify(D()) // CHECK: --not fooable--

extension Int: Fooable {
  func foo() { println("--Int--") }
}

fooify(1) // CHECK: --Int--
fooify(1) // CHECK: --Int--

extension D: Fooable {
  func foo() { println("--D--") }
}

fooify(D()) // CHECK: D
fooify(D()) // CHECK: D
fooify(C()) // CHECK: --not fooable--
fooify(C()) // CHECK: --not fooable--
