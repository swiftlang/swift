// RUN: %target-typecheck-verify-swift -swift-version 5

struct S0<T> {
  func foo(_ other: Self) { }
}

class C0<T> {
  func foo(_ other: Self) { } // expected-error{{covariant 'Self' can only appear as the type of a property, subscript or method result; did you mean 'C0'?}}
}

enum E0<T> {
  func foo(_ other: Self) { }
}

// rdar://problem/21745221
struct X {
  typealias T = Int
}

extension X {
  struct Inner {
  }
}

extension X.Inner {
  func foo(_ other: Self) { }
}

// SR-695
class Mario {
  func getFriend() -> Self { return self } // expected-note{{overridden declaration is here}}
  func getEnemy() -> Mario { return self }
}
class SuperMario : Mario {
  override func getFriend() -> SuperMario { // expected-error{{cannot override a Self return type with a non-Self return type}}
    return SuperMario()
  }
  override func getEnemy() -> Self { return self }
}
final class FinalMario : Mario {
    override func getFriend() -> FinalMario {
        return FinalMario()
    }
}

// These references to Self are now possible (SE-0068)

class A<T> {
  typealias _Self = Self
  // expected-error@-1 {{covariant 'Self' can only appear as the type of a property, subscript or method result; did you mean 'A'?}}
  let b: Int
  required init(a: Int) {
    print("\(Self.self).\(#function)")
    Self.y()
    b = a
  }
  static func z(n: Self? = nil) {
    // expected-error@-1 {{covariant 'Self' can only appear as the type of a property, subscript or method result; did you mean 'A'?}}
    print("\(Self.self).\(#function)")
  }
  class func y() {
    print("\(Self.self).\(#function)")
    Self.z()
  }
  func x() -> A? {
    print("\(Self.self).\(#function)")
    Self.y()
    Self.z()
    let _: Self = Self.init(a: 66)
    return Self.init(a: 77) as? Self as? A
    // expected-warning@-1 {{conditional cast from 'Self' to 'Self' always succeeds}}
    // expected-warning@-2 {{conditional downcast from 'Self?' to 'A<T>' is equivalent to an implicit conversion to an optional 'A<T>'}}
  }
  func copy() -> Self {
    let copy = Self.init(a: 11)
    return copy
  }

  var copied: Self {
    let copy = Self.init(a: 11)
    return copy
  }
  subscript (i: Int) -> Self { // expected-error {{mutable subscript cannot have covariant 'Self' type}}
    get {
      return Self.init(a: i)
    }
    set(newValue) {
    }
  }
}

class B: A<Int> {
  let a: Int
  required convenience init(a: Int) {
    print("\(Self.self).\(#function)")
    self.init()
  }
  init() {
    print("\(Self.self).\(#function)")
    Self.y()
    Self.z()
    a = 99
    super.init(a: 88)
  }
  override class func y() {
    print("override \(Self.self).\(#function)")
  }
  override func copy() -> Self {
    let copy = super.copy() as! Self // supported
    return copy
  }
  override var copied: Self {
    let copy = super.copied as! Self // unsupported
    return copy
  }
}

class C {
  required init() {
  }

  func f() {
    func g(_: Self) {}
    let x: Self = self as! Self
    g(x)
    typealias _Self = Self
  }
  func g() {
    _ = Self.init() as? Self
    // expected-warning@-1 {{conditional cast from 'Self' to 'Self' always succeeds}}
  }
  func h(j: () -> Self) -> () -> Self {
    // expected-error@-1 {{covariant 'Self' can only appear at the top level of method result type}}
    return { return self }
  }
  func i() -> (Self, Self) {}
  // expected-error@-1 {{covariant 'Self' can only appear at the top level of method result type}}

  func j() -> Self.Type {}
  // expected-error@-1 {{covariant 'Self' can only appear at the top level of method result type}}

  let p0: Self? // expected-error {{stored property cannot have covariant 'Self' type}}
  var p1: Self? // expected-error {{stored property cannot have covariant 'Self' type}}

  static func staticFunc() -> Self {}
  let stored: Self = Self.staticFunc() // expected-error {{stored property cannot have covariant 'Self' type}}
  // expected-error@-1 {{covariant 'Self' type cannot be referenced from a stored property initializer}}

  var prop: Self { // expected-error {{mutable property cannot have covariant 'Self' type}}
    get {
      return self
    }
    set (newValue) {
    }
  }
  subscript (i: Int) -> Self { // expected-error {{mutable subscript cannot have covariant 'Self' type}}
    get {
      return self
    }
    set (newValue) {
    }
  }
}

struct S1 {
  typealias _SELF = Self
  let j = 99.1
  subscript (i: Int) -> Self {
    get {
      return self
    }
    set(newValue) {
    }
  }
  var foo: Self {
    get {
      return self// as! Self
    }
    set (newValue) {
    }
  }
  func x(y: () -> Self, z: Self) {
  }
}

struct S2 {
  let x = 99
  struct S3<T> {
    let x = 99
    static func x() {
      Self.y()
    }
    func f() {
      func g(_: Self) {}
    }
    static func y() {
      print("HERE")
    }
    func foo(a: [Self]) -> Self? {
      Self.x()
      return self as? Self
      // expected-warning@-1 {{conditional cast from 'S2.S3<T>' to 'S2.S3<T>' always succeeds}}
    }
  }
  func copy() -> Self {
    let copy = Self.init()
    return copy
  }

  var copied: Self {
    let copy = Self.init()
    return copy
  }
}

extension S2 {
  static func x() {
    Self.y()
  }
  static func y() {
    print("HERE")
  }
  func f() {
    func g(_: Self) {}
  }
  func foo(a: [Self]) -> Self? {
    Self.x()
    return Self.init() as? Self
    // expected-warning@-1 {{conditional cast from 'S2' to 'S2' always succeeds}}
  }
  subscript (i: Int) -> Self {
    get {
      return Self.init()
    }
    set(newValue) {
    }
  }
}

enum E {
  static func f() {
    func g(_: Self) {}
    print("f()")
  }
  case e
  func h(h: Self) -> Self {
    Self.f()
    return .e
  }
}

class SelfStoredPropertyInit {
  static func myValue() -> Int { return 123 }

  var value = Self.myValue() // expected-error {{covariant 'Self' type cannot be referenced from a stored property initializer}}
}

// rdar://problem/55273931 - erroneously rejecting 'Self' in lazy initializer
class Foo {
  static var value: Int = 17

  lazy var doubledValue: Int = {
    Self.value * 2
  }()
}
