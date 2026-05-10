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
  // TODO(diagnostics): Since `A` and `B` are related it would make sense to suggest forced downcast.
  foo7(b : a) // expected-error {{cannot convert value of type '[A]' to expected argument type '[B]'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('A' and 'B') are expected to be equal}}
}

protocol P1 {}
struct S1 : P1 {}
func foo9(s : S1) {}
func foo10(p : P1) {
  foo9(s : p) // expected-error {{cannot convert value of type 'any P1' to expected argument type 'S1'}} {{13-13= as! S1}}
}

func foo11(a : [AnyHashable]) {}
func foo12(b : [NSObject]) {
  foo11(a : b)
}

func foo13(a : [AnyHashable : Any]) {}
func foo14(b : [NSObject : AnyObject]) {
  foo13(a : b)
}


// Add a minimal test for inout-to-pointer conversion involving a
// generic function with a protocol constraint of Equatable.
infix operator =*= : ComparisonPrecedence
func =*= <T : Equatable>(lhs: T, rhs: T) -> Bool {
 return lhs == rhs
}
func =*= <T : Equatable>(lhs: T?, rhs: T?) -> Bool {
 return lhs == rhs
}

class C {}

var o = C()
var p: UnsafeMutablePointer<C>? = nil

_ = p =*= &o


func rdar25963182(_ bytes: [UInt8] = nil) {}
// expected-error@-1 {{nil default argument value cannot be converted to type}}

// https://github.com/apple/swift/issues/55702
do {
  struct S {}

  func returnVoid(_ x: Int) {}
  func returnInt(_ x: Int) -> Int {}
  func returnS(_ x: Int) -> S {}

  let arr: [Int]

  for x in arr where returnVoid(x) {} // expected-error {{cannot convert value of type '()' to expected condition type 'Bool'}}
  for x in arr where returnInt(x) {} // expected-error {{type 'Int' cannot be used as a boolean; test for '!= 0' instead}} {{22-22=(}} {{34-34= != 0)}}
  for x in arr where returnS(x) {} // expected-error {{cannot convert value of type 'S' to expected condition type 'Bool'}}
}
