// RUN: %swift -parse -verify %s

struct A {
  func foo() {}
}

var a : A?
var b : A ? // expected-error {{consecutive statements on a line}} expected-error {{expected expression}}

var c = a?
var d = a?.foo()

var e : (() -> A)?
var f = e?()

