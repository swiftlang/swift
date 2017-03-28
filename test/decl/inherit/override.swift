// RUN: %target-typecheck-verify-swift -parse-as-library -swift-version 4

@objc class ObjCClassA {}
@objc class ObjCClassB : ObjCClassA {}

class A { 
  func f1() { } // expected-note{{overridden declaration is here}}
  func f2() -> A { } // expected-note{{overridden declaration is here}}

  @objc func f3() { } // expected-note{{overridden declaration is here}}
  @objc func f4() -> ObjCClassA { } // expected-note{{overridden declaration is here}}
  @objc var v1: Int { return 0 } // expected-note{{overridden declaration is here}}
  @objc var v2: Int { return 0 } // expected-note{{overridden declaration is here}}
  @objc var v3: Int = 0 // expected-note{{overridden declaration is here}}

  dynamic func f3D() { } // expected-error{{'dynamic' instance method 'f3D()' must also be '@objc'}}{{3-3=@objc }}
  dynamic func f4D() -> ObjCClassA { } // expected-error{{'dynamic' instance method 'f4D()' must also be '@objc'}}{{3-3=@objc }}
}

extension A {
  func f5() { } // expected-note{{overridden declaration is here}}
  func f6() -> A { } // expected-note{{overridden declaration is here}}

  @objc func f7() { }
  @objc func f8() -> ObjCClassA { }
}

class B : A { }

extension B { 
  func f1() { }  // expected-error{{declarations in extensions cannot override yet}}
  func f2() -> B { } // expected-error{{declarations in extensions cannot override yet}}

  override func f3() { } // expected-error{{cannot override a non-dynamic class declaration from an extension}}
  override func f4() -> ObjCClassB { } // expected-error{{cannot override a non-dynamic class declaration from an extension}}
  override var v1: Int { return 1 } // expected-error{{cannot override a non-dynamic class declaration from an extension}}
  override var v2: Int { // expected-error{{cannot override a non-dynamic class declaration from an extension}}
    get { return 1 }
    set { }
  }
  override var v3: Int { // expected-error{{cannot override a non-dynamic class declaration from an extension}}
    willSet { }
    didSet { }
  }

  override func f3D() { }
  override func f4D() -> ObjCClassB { }

  func f5() { }  // expected-error{{declarations in extensions cannot override yet}}
  func f6() -> A { }  // expected-error{{declarations in extensions cannot override yet}}

  @objc override func f7() { }
  @objc override func f8() -> ObjCClassA { }
}

func callOverridden(_ b: B) {
  b.f3()
  _ = b.f4()
  b.f7()
  _ = b.f8()
}

@objc
class Base {
  func meth(_ x: Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
}
@objc
class Sub : Base {
  func meth(_ x: Undeclared) {} // expected-error {{use of undeclared type 'Undeclared'}}
}

// Objective-C method overriding

@objc class ObjCSuper {
  func method(_ x: Int, withInt y: Int) { }

  func method2(_ x: Sub, withInt y: Int) { }

  @objc func method3(_ x: Base, withInt y: Int) { } // expected-note{{method 'method3(_:withInt:)' declared here}}
}

class ObjCSub : ObjCSuper {
  override func method(_ x: Int, withInt y: Int) { } // okay, overrides exactly

  override func method2(_ x: Base, withInt y: Int) { } // okay, overrides trivially

  @objc(method3:withInt:) func method3(_ x: Sub, with y: Int) { } // expected-error{{method3(_:with:)' with Objective-C selector 'method3:withInt:' conflicts with method 'method3(_:withInt:)' from superclass 'ObjCSuper' with the same Objective-C selector}}
}
