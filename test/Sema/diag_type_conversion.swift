// RUN: %target-typecheck-verify-swift %clang-importer-sdk

// REQUIRES: objc_interop

import Foundation

func foo1(_ a: [Int]) {}
func foo2(_ a : UnsafePointer<Int>) {}
func foo4(_ a : UnsafeMutablePointer<Int>) {}
func foo3 () {
  let j = 3
  foo2(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{none}}
  foo4(j) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{none}}

  var i = 3
  foo2(i) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{8-8=&}}
  foo4(i) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{8-8=&}}

  foo2(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafePointer<Int>'}} {{none}}
  foo4(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UnsafeMutablePointer<Int>'}} {{none}}
}

class A {}
class B : A {}
func foo5(b : B) {}
func foo6(a : A) {
  foo5(b : a) // expected-error {{cannot convert value of type 'A' to expected argument type 'B'}} {{13-13= as! B}}
}

func foo7(b : [B]) {}
func foo8(a : [A]) {
  foo7(b : a) // expected-error {{cannot convert value of type '[A]' to expected argument type '[B]'}} {{13-13= as! [B]}}
}

protocol P1 {}
struct S1 : P1 {}
func foo9(s : S1) {}
func foo10(p : P1) {
  foo9(s : p) // expected-error {{cannot convert value of type 'P1' to expected argument type 'S1'}} {{13-13= as! S1}}
}

func foo11(a : [AnyHashable]) {}
func foo12(b : [NSObject]) {
  foo11(a : b)
}

func foo13(a : [AnyHashable : Any]) {}
func foo14(b : [NSObject : AnyObject]) {
  foo13(a : b)
}
