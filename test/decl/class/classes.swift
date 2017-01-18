// RUN: %target-typecheck-verify-swift -parse-as-library

class B : A {
  override init() { super.init() }
  override func f() {}
  func g() -> (B, B) { return (B(), B()) } // expected-error {{declaration 'g()' cannot override more than one superclass declaration}}
  override func h() -> (A, B) { return (B(), B()) } // expected-note {{'h()' previously overridden here}}
  override func h() -> (B, A) { return (B(), B()) } // expected-error {{'h()' has already been overridden}}
  func i() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  override func j() -> Int { return 0 }
  func j() -> Float { return 0.0 }
  func k() -> Float { return 0.0 }
  override func l(_ l: Int) {}
  override func l(_ l: Float) {}
  override func m(_ x: Int) {}
  func m(_ x: Float) {} // not an override of anything
  func n(_ x: Float) {}
  override subscript(i : Int) -> Int {
    get {}
    set {}
  }
}
class A {
  init() { }
  func f() {}
  func g() -> (B, A) { return (B(), B()) } // expected-note{{overridden declaration is here}}
  func g() -> (A, B) { return (B(), B()) } // expected-note{{overridden declaration is here}}
  func h() -> (A, A) { return (B(), B()) }
  func j() -> Int { return 0 }
  func k() -> Int { return 0 }
  func l(_ l: Int) {}
  func l(_ l: Float) {}
  func m(_ x: Int) {}
  func m(y: Int) {}
  func n(_ x: Int) {}
  subscript(i : Int) -> Int {
    get {}
    set {}
  }
}
extension A {
  func i() {} // expected-note{{overridden declaration is here}}
}
func f() {
  let x = B()
  _ = x.f() as ()
  _ = x[10] as Int
}

class C<T> {
  init() { }
  func f(_ v: T) -> T { return v }
}
class D : C<Int> {
  override init() { super.init() }
  override func f(_ v: Int) -> Int { return v+1 }
}
func f2() {
  let x = D()
  _ = x.f(10)
}

class E<T> {
  func f(_ v: T) -> T { return v }
}
class F : E<Int> {}
class G : F {
    override func f(_ v: Int) -> Int { return v+1 }
}

// Explicit downcasting
func test_explicit_downcasting(_ f: F, ei: E<Int>) {
  var g = f as! G
  g = ei as! G
  _ = g
}

// Type and instance functions with the same name
class H {
  func f(_ x: Int) { }
  class func f(_ x: Int) { }
}

class HDerived : H {
  override func f(_ x: Int) { }
  override class func f(_ x: Int) { }
}
