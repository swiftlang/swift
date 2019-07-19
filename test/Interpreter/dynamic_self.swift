// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// Test IR generation via execution for Self.

protocol P {
  func f() -> Self
  func g() -> Self
}

protocol CP : class {
  func f() -> Self
  func g() -> Self
}

func callDynamicSelfExistential(_ p: P) {
  print("Before first call")
  var p2 = p.f()
  print("Between calls")
  p2.g()
  print("After second call")
}

func callDynamicSelfClassExistential(_ cp: CP) {
  print("Before first call")
  var cp2 = cp.f()
  print("Between calls")
  cp2.g()
  print("After second call")
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
}

class C : P, CP {
  init() {
    print("Allocating C")
  }

  func f() -> Self {
    print("C.f()")
    return self
  }

  func g() -> Self {
    print("C.g()")
    return self
  }
}

print("-------------------------------")

// CHECK: S() as non-class existential
print("S() as non-class existential")
// CHECK-NEXT: Before first call
// CHECK-NEXT: S.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: S.g()
// CHECK-NEXT: After second call
callDynamicSelfExistential(S())

// CHECK-NEXT: C() as non-class existential
print("C() as non-class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
callDynamicSelfExistential(C())

// CHECK-NEXT: C() as class existential
print("C() as class existential")
// CHECK-NEXT: Allocating C
// CHECK-NEXT: Before first call
// CHECK-NEXT: C.f()
// CHECK-NEXT: Between calls
// CHECK-NEXT: C.g()
// CHECK-NEXT: After second call
callDynamicSelfClassExistential(C())

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
