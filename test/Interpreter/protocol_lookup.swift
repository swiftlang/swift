// Check both AOT and JIT modes
// RUN: %target-run-simple-swift | FileCheck %s
// RUN: %swift -interpret %s | FileCheck %s
// TODO: Write these using "x as P" casts when we support that.

@asmname("swift_stdlib_dynamicCastToExistential1_2")
func castToProtocol<SourceType, DestType>(
    value: SourceType,
    _: DestType.Type
) -> DestType?

protocol Fooable {
  func foo()
}

struct S: Fooable {
  func foo() { println("S") }
}

class C: Fooable {
  func foo() { println("C") }
}

class D: C {
  override func foo() { println("D") }
}

class E: D {
  override func foo() { println("E") }
}

struct X {}

extension Int: Fooable {
  func foo() { println("Int") }
}

func fooify<T>(x: T) {
  if let foo = castToProtocol(x, Fooable.self) {
    foo.foo()
  } else {
    println("not fooable")
  }
}

struct G<T>: Fooable {
  func foo() { println("G") }
}

struct H<T> {}

fooify(1)   // CHECK:      Int
fooify(2)   // CHECK-NEXT: Int
fooify(S()) // CHECK-NEXT: S
fooify(S()) // CHECK-NEXT: S
fooify(C()) // CHECK-NEXT: C
fooify(C()) // CHECK-NEXT: C
// TODO: Subclasses
fooify(D()) // CHECK-NEXT: D
fooify(D()) // CHECK-NEXT: D
fooify(E()) // CHECK-NEXT: E
fooify(E()) // CHECK-NEXT: E
fooify(X()) // CHECK-NEXT: not fooable
fooify(X()) // CHECK-NEXT: not fooable
fooify(G<Int>()) // CHECK-NEXT: G
fooify(G<Float>()) // CHECK-NEXT: G
fooify(G<Int>()) // CHECK-NEXT: G
fooify(H<Int>()) // CHECK-NEXT: not fooable
fooify(H<Float>()) // CHECK-NEXT: not fooable
fooify(H<Int>()) // CHECK-NEXT: not fooable

// TODO: generics w/ dependent witness tables
// TODO: imported value types
// TODO: objc classes
// TODO: cf classes
// TODO: objc protocols
