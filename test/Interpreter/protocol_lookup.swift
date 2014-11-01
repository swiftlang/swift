// RUN: %target-run-simple-swift
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

fooify(1)   // CHECK:      Int
fooify(2)   // CHECK-NEXT: Int
fooify(S()) // CHECK-NEXT: S
fooify(S()) // CHECK-NEXT: S
fooify(C()) // CHECK-NEXT: C
fooify(C()) // CHECK-NEXT: C
// TODO: Subclasses
fooify(D()) // TODO CHECK-NEXT: not fooable
fooify(D()) // TODO CHECK-NEXT: not fooable
fooify(X()) // CHECK-NEXT: not fooable
fooify(X()) // CHECK-NEXT: not fooable

// TODO: generics
// TODO: imported value types
// TODO: subclasses
// TODO: objc classes
// TODO: cf classes
// TODO: objc protocols
// TODO: jit
