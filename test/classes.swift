// RUN: %swift -parse %s -verify -parse-as-library

class B : A {
  func f() {}
  func g() -> (B, B) { return (new B, new B) } // expected-error {{declaration cannot override more than one base class declaration}}
  func h() -> (A, B) { return (new B, new B) }
  func h() -> (B, A) { return (new B, new B) } // expected-error {{declaration cannot be overridden by more than one derived class declaration}}
  func i() {} // expected-error {{declarations from extensions cannot be overridden yet}}
  subscript(i : Int) -> Int { get {} set {} }
}
class A {
  func f() {}
  func g() -> (B, A) { return (new B, new B) }
  func g() -> (A, B) { return (new B, new B) }
  func h() -> (A, A) { return (new B, new B) } 
  subscript(i : Int) -> Int { get {} set {} }
}
extension A {
  func i() {}
}
func f() {
  var x = new B
  var y : () = x.f()
  var z : Int = x[10]
}
