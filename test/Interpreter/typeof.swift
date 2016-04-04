// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

protocol Fooable { static func foo() }

class B : Fooable {
  class func foo() { print("Beads?!") }
}

class D : B {
  override class func foo() { print("Deeds?!") }
}

struct S : Fooable {
  static func foo() { print("Seeds?!") }
}

func classMetatype(_ b: B.Type) {
  b.foo()
}

func structMetatype(_ s: S.Type) {
  s.foo()
}

func archeMetatype<T : Fooable>(_ t: T.Type) {
  t.foo()
}

func archeMetatype2<T : Fooable>(_ t: T) {
  t.dynamicType.foo()
}

func boxedExistentialMetatype(_ e: ErrorProtocol) -> ErrorProtocol.Type {
  return e.dynamicType
}

enum Hangry : ErrorProtocol {
  case Hungry, Angry
}

class Meltdown : ErrorProtocol {
  var _domain : String {
    return "_domain"
  }

  var _code : Int {
    return 420
  }
}

class GrilledCheese : Meltdown {}

// CHECK: Beads?
classMetatype(B().dynamicType)
// CHECK: Deeds?
classMetatype(D().dynamicType)

// CHECK: Seeds?
structMetatype(S().dynamicType)

// CHECK: Beads?
archeMetatype(B().dynamicType)
// FIXME: Deeds? <rdar://problem/14620454>
archeMetatype(D().dynamicType)
// CHECK: Seeds?
archeMetatype(S().dynamicType)

// CHECK: Beads?
archeMetatype2(B())
// FIXME: Deeds? <rdar://problem/14620454>
archeMetatype2(D())
// CHECK: Seeds?
archeMetatype2(S())

// CHECK: Hangry
print(boxedExistentialMetatype(Hangry.Hungry))
// CHECK: Meltdown
print(boxedExistentialMetatype(Meltdown()))
// CHECK: GrilledCheese
print(boxedExistentialMetatype(GrilledCheese()))
// CHECK: GrilledCheese
print(boxedExistentialMetatype(GrilledCheese() as Meltdown))
