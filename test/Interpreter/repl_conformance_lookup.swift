// RUN: %swift -repl < %s | FileCheck %s
// REQUIRES: swift_repl
// TODO: Write these using "x as P" casts when we support that.

@asmname("swift_stdlib_dynamicCastToExistential1_2") func castToProtocol<SourceType, DestType>(
    value: SourceType,
    _: DestType.Type
) -> DestType?;

protocol Fooable {
  func foo()
}

class C {}
class D: C {}

func fooify<T>(x: T) {
  if let foo = castToProtocol(x, Fooable.self) {
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
