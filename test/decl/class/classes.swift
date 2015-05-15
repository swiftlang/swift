// RUN: %target-parse-verify-swift -parse-as-library

class B : A {
  override init() { super.init() }
  override func f() {}
  func g() -> (B, B) { return (B(), B()) } // expected-error {{declaration 'g()' cannot override more than one superclass declaration}}
  override func h() -> (A, B) { return (B(), B()) }
  override func h() -> (B, A) { return (B(), B()) }
  func i() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  override func j() -> Int { return 0 }
  func j() -> Float { return 0.0 }
  func k() -> Float { return 0.0 }
  override func l(l: Int) {}
  override func l(l: Float) {}
  override func m(x: Int) {}
  func m(x: Float) {} // not an override of anything
  func n(x: Float) {}
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
  func l(l: Int) {}
  func l(l: Float) {}
  func m(x: Int) {}
  func m(y y: Int) {}
  func n(x: Int) {}
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
  func f(v: T) -> T { return v }
}
class D : C<Int> {
  override init() { super.init() }
  override func f(v: Int) -> Int { return v+1 }
}
func f2() {
  let x = D()
  _ = x.f(10)
}

class E<T> {
  func f(v: T) -> T { return v }
}
class F : E<Int> {}
class G : F {
    override func f(v: Int) -> Int { return v+1 }
}

// Explicit downcasting
func test_explicit_downcasting(f: F, ei: E<Int>) {
  var g = f as! G
  g = ei as! G
  _ = g
}

// Type and instance functions with the same name
class H {
  func f(x: Int) { }
  class func f(x: Int) { }
}

class HDerived : H {
  override func f(x: Int) { }
  override class func f(x: Int) { }
}

