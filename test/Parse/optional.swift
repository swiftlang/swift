// RUN: %target-parse-verify-swift

struct A {
  func foo() {}
}

var a : A?
var b : A ? // expected-error {{consecutive statements on a line}} expected-error {{expected expression}}

var c = a?  // expected-error {{'?' must be followed by a call, member lookup, or subscript}}
var d : ()? = a?.foo()

var e : (() -> A)?
var f = e?()

struct B<T> {}
var g = B<A!>()
