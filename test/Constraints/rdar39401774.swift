// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

class A<T> {
  var foo: Int? { return 42 }
  func baz() -> T { fatalError() }
  func fiz() -> Int { return 42 }
}

protocol P1 {
  associatedtype T
  var foo: Int? { get }
  func baz() -> T
  func fiz() -> Int
}

protocol P2 : P1 {
  var bar: Int? { get }
}

extension P2 where Self: A<Int> {
  var bar: Int? {
    // CHECK: %3 = upcast %0 : $Self to $A<Int>
    // CHECK-NEXT: class_method %3 : $A<Int>, #A.foo!getter.1 : <T> (A<T>) -> () -> Int?
    guard let foo = foo else { return 0 } // expected-warning {{value 'foo' was defined but never used}}
    // CHECK: %15 = upcast %0 : $Self to $A<Int>
    // CHECK-NEXT: class_method %15 : $A<Int>, #A.baz!1 : <T> (A<T>) -> () -> T
    let _ = baz()
    // CHECK: %22 = upcast %0 : $Self to $A<Int>
    // CHECK-NEXT: class_method %22 : $A<Int>, #A.fiz!1 : <T> (A<T>) -> () -> Int
    return fiz()
  }
}
