// RUN: %target-run-simple-swift | %FileCheck %s
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
  type(of: t).foo()
}

func boxedExistentialMetatype(_ e: Error) -> Error.Type {
  return type(of: e)
}

enum Hangry : Error {
  case Hungry, Angry
}

class Meltdown : Error {
  var _domain : String {
    return "_domain"
  }

  var _code : Int {
    return 420
  }
}

class GrilledCheese : Meltdown {}

func labeledTuple() -> (x: Int, y: Int, Double) {
  return (x: 1, y: 1, 3.14159)
}

// CHECK: Beads?
classMetatype(type(of: B()))
// CHECK: Deeds?
classMetatype(type(of: D()))

// CHECK: Seeds?
structMetatype(type(of: S()))

// CHECK: Beads?
archeMetatype(type(of: B()))
// FIXME: Deeds? <rdar://problem/14620454>
archeMetatype(type(of: D()))
// CHECK: Seeds?
archeMetatype(type(of: S()))

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
// CHECK: (x : Int, y : Int, Double)
print(type(of: labeledTuple()))
