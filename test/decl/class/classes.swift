// RUN: %swift -parse %s -verify -parse-as-library

class B : A {
  init() { super.init() }
  def f() {}
  def g() -> (B, B) { return (B(), B()) } // expected-error {{declaration cannot override more than one superclass declaration}}
  def h() -> (A, B) { return (B(), B()) }
  def h() -> (B, A) { return (B(), B()) } // expected-error {{declaration cannot be overridden by more than one subclass declaration}}
  def i() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  def j() -> Int { return 0 } 
  def j() -> Float { return 0.0 } 
  def k() -> Float { return 0.0 } // expected-error {{cannot overload a declaration from a superclass}}
  def l(l: Int) {}
  def l(ll: Float) {}
  def m(x: Int) {}
  def m(z: Int) {}
  subscript(i : Int) -> Int { get: set: }
}
class A {
  init() { }
  def f() {}
  def g() -> (B, A) { return (B(), B()) }
  def g() -> (A, B) { return (B(), B()) }
  def h() -> (A, A) { return (B(), B()) }
  def j() -> Int { return 0 } 
  def k() -> Int { return 0 }
  def l(l: Int) {}
  def l(l: Float) {}
  def m(x: Int) {}
  def m(y: Int) {}
  subscript(i : Int) -> Int { get: set: }
}
extension A {
  def i() {} // expected-note{{overridden declaration is here}}
}
def f() {
  var x = B()
  var y : () = x.f()
  var z : Int = x[10]
}

class C<T> {
  init() { }
  def f(v: T) -> T { return v }
}     
class D : C<Int> {
  init() { super.init() }
  def f(v: Int) -> Int { return v+1 }
}
def f2() {
  var x = D()
  var y = x.f(10)
}

class E<T> {
  def f(v: T) -> T { return v }
}
class F : E<Int> {}
class G : F {
    def f(v: Int) -> Int { return v+1 }
}

// Explicit downcasting
def test_explicit_downcasting(f: F, ei: E<Int>) {
  var g = (f as G)!
  g = (ei as G)!
}
