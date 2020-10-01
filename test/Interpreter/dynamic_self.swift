// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Test IR generation via execution for Self.

protocol P {
  func f() -> Self
  func g() -> Self
  subscript() -> Self { get }
  var p: Self { get }
}

protocol CP : class {
  func f() -> Self
  func g() -> Self
  subscript() -> Self { get }
  var p: Self { get }
}

extension P {
  func f() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] P extension.f()")
    return self
  }
  func g() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] P extension.g()")
    return self
  }
  subscript() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] P extension.subscript()")
    return self
  }
  var p: Self {
    print("[Self := \(Self.self), self is \(type(of: self))] P extension.p")
    return self
  }
}

extension CP {
  func f() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] CP extension.f()")
    return self
  }
  func g() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] CP extension.g()")
    return self
  }
  subscript() -> Self {
    print("[Self := \(Self.self), self is \(type(of: self))] CP extension.subscript()")
    return self
  }
  var p: Self {
    print("[Self := \(Self.self), self is \(type(of: self))] CP extension.p")
    return self
  }
}

func callDynamicSelfExistential(_ p: P) {
  print("callDynamicSelfExistential {")
  let p2 = p.f()
  let p3 = p2.g()
  let p4 = p3[]
  let p5 = p4.p
  print(" } callDynamicSelfExistential")
}

func callDynamicSelfClassExistential(_ cp: CP) {
  print("callDynamicSelfClassExistential {")
  let cp2 = cp.f()
  let cp3 = cp2.g()
  let cp4 = cp3[]
  let cp5 = cp4.p
  print(" } callDynamicSelfClassExistential")
}

struct S : P {
  func f() -> S {
    print("S.f()")
    return self
  }

  func g() -> S {
    print("S.g()")
    return self
  }

  subscript() -> S {
    print("S.subscript()")
    return self
  }

  var p: S {
    print("S.p")
    return self
  }
}

class C1a : P, CP {
  func f() -> Self {
    print("C1a.f()")
    return self
  }

  func g() -> Self {
    print("C1a.g()")
    return self
  }

  subscript() -> Self {
    print("C1a.subscript()")
    return self
  }

  var p: Self {
    print("C1a.p")
    return self
  }
}
final class C1b : C1a {
  override subscript() -> Self {
    print("C1b.subscript()")
    return self
  }
}

class C2a : P {}
final class C2b : C2a {}

class C3a : CP {}
final class C3b : C3a {}

print("-------------------------------")

// CHECK: callDynamicSelfExistential {
// CHECK-NEXT: S.f()
// CHECK-NEXT: S.g()
// CHECK-NEXT: S.subscript()
// CHECK-NEXT: S.p
// CHECK-NEXT: } callDynamicSelfExistential
callDynamicSelfExistential(S())

// CHECK-NEXT: callDynamicSelfExistential {
// CHECK-NEXT: C1a.f()
// CHECK-NEXT: C1a.g()
// CHECK-NEXT: C1a.subscript()
// CHECK-NEXT: C1a.p
// CHECK-NEXT: } callDynamicSelfExistential
callDynamicSelfExistential(C1a())

// CHECK-NEXT: callDynamicSelfExistential {
// CHECK-NEXT: C1a.f()
// CHECK-NEXT: C1a.g()
// CHECK-NEXT: C1b.subscript()
// CHECK-NEXT: C1a.p
// CHECK-NEXT: } callDynamicSelfExistential
callDynamicSelfExistential(C1b())

// CHECK-NEXT: callDynamicSelfExistential {
// CHECK-NEXT: [Self := C2a, self is C2b] P extension.f()
// CHECK-NEXT: [Self := C2a, self is C2b] P extension.g()
// CHECK-NEXT: [Self := C2a, self is C2b] P extension.subscript()
// CHECK-NEXT: [Self := C2a, self is C2b] P extension.p
// CHECK-NEXT: } callDynamicSelfExistential
callDynamicSelfExistential(C2b() as C2a)

// CHECK-NEXT: callDynamicSelfClassExistential {
// CHECK-NEXT: C1a.f()
// CHECK-NEXT: C1a.g()
// CHECK-NEXT: C1a.subscript()
// CHECK-NEXT: C1a.p
// CHECK-NEXT: } callDynamicSelfClassExistential
callDynamicSelfClassExistential(C1a())

// CHECK-NEXT: callDynamicSelfClassExistential {
// CHECK-NEXT: [Self := C3b, self is C3b] CP extension.f()
// CHECK-NEXT: [Self := C3b, self is C3b] CP extension.g()
// CHECK-NEXT: [Self := C3b, self is C3b] CP extension.subscript()
// CHECK-NEXT: [Self := C3b, self is C3b] CP extension.p
// CHECK-NEXT: } callDynamicSelfClassExistential
callDynamicSelfClassExistential(C3b() as C3a)

print("-------------------------------")

class Z {
  let name: String

  init(name: String) {
    self.name = name
  }

  func testCaptures(x: Int) -> Self {
    let fn1 = {
      print("First: \(self.name)")
    }
    fn1()

    let fn2 = { [weak self] in
      if let strongSelf = self {
        print("Second: \(strongSelf.name)")
      }
    }
    fn2()

    let fn3 = {
      print("Third: \(self.name)")
      print("Third: \(x)")
    }
    fn3()

    return self
  }

}

// CHECK: First: Leeloo
// CHECK-NEXT: Second: Leeloo
// CHECK-NEXT: Third: Leeloo
// CHECK-NEXT: Third: 42
Z(name: "Leeloo").testCaptures(x: 42)

print("-------------------------------")

func makeInstance<T: Base>(_: T.Type) -> T {
    return T()
}

@_transparent
func makeInstanceTransparent<T: Base>(_: T.Type) -> T {
    return T()
}

@_transparent
func makeInstanceTransparentProtocol<T: Base>(_: T.Type) -> T {
    return T()
}

protocol Factory {
  init()
}

class Base : Factory {
  required init() {}

  func returnsNewInstance() -> Self {
    return makeInstance(type(of: self))
  }

  func returnsNewInstanceTransparent() -> Self {
    return makeInstanceTransparent(type(of: self))
  }

  func returnsNewInstanceTransparentProtocol() -> Self {
    return makeInstanceTransparentProtocol(type(of: self))
  }
}

class Derived : Base { }

// CHECK: main.Base
// CHECK: main.Base
// CHECK: main.Base
print(Base().returnsNewInstance())
print(Base().returnsNewInstanceTransparent())
print(Base().returnsNewInstanceTransparentProtocol())

// CHECK: main.Derived
// CHECK: main.Derived
// CHECK: main.Derived
print(Derived().returnsNewInstance())
print(Derived().returnsNewInstanceTransparent())
print(Derived().returnsNewInstanceTransparentProtocol())

// CHECK: main.Derived
// CHECK: main.Derived
// CHECK: main.Derived
print((Derived() as Base).returnsNewInstance())
print((Derived() as Base).returnsNewInstanceTransparent())
print((Derived() as Base).returnsNewInstanceTransparentProtocol())


// Read-only properties and subscripts returning Self

open class A1<T> {
  let i: Int
  public required init(i: Int) {
    self.i = i
  }
  func copy() -> Self {
    let copy = Self.init(i: 81)
    return copy
  }

  open var copied: Self {
    let copy = Self.init(i: 82)
    return copy
  }
  open subscript (i: Int) -> Self {
    return Self.init(i: 80+i)
  }
}

class B1: A1<Int> {
  let j = 88
  override func copy() -> Self {
    let copy = super.copy() as! Self // supported
    return copy
  }
  override var copied: Self {
    let copy = Self.init(i: 99)
    return copy
  }
  //  override subscript (i: Int) -> Self {
  //    return Self.init(i: i+1000)
  //  }
}

// CHECK: 181
// CHECK: 88
// CHECK: 88
// CHECK: 82
// CHECK: 99
print(A1<Int>(i: 100)[101].i)
print(B1(i: 100)[101].j)
print(B1(i: 100).copy().j)
print(A1<Int>(i: 100).copied.i)
print(B1(i: 100).copied.i)

class A0<T, S> {
  var i = "Base"
  required init() {}

  func copy() -> Self {
    let copy = Self.init()
    return copy
  }

  var copied: Self {
    get {
      let copy = Self.init()
      return copy
    }
  }
  open subscript (i: Int) -> Self {
    get {
      return Self.init()
    }
  }
}

protocol Prot3 {
  static func +(x: Self, y: Self) -> String
}

class B: A0<Int, Double>, Prot3 {
  var j = "Derived"
  static func + (x: B, y: B) -> String {
    return x.j + x.j
  }
  override func copy() -> Self {
    let copy = Self.init()
    return copy
  }
  override var copied: Self {
    get {
      return copy() as! Self
    }
  }
  //  override subscript (i: Int) -> Self {
  //    return Self.init()
  //  }
}

// CHECK: DerivedDerived
// CHECK: DerivedDerived
print(B()[0][0].j + B().copied.copied.j)
print(B()[0][0] + B().copied.copied)

// CHECK-NEXT: Done
print("Done")
