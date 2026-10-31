// RUN: %target-typecheck-verify-swift

struct A {
  func foo() {}
}

var a : A?
var b : A ? // expected-cparser-error {{consecutive statements on a line}} {{10-10=;}} expected-cparser-error {{expected expression}} expected-astgen-error {{unexpected code '?' in source file}}

var c = a?  // expected-error {{'?' must be followed by a call, member lookup, or subscript}}
var d : ()? = a?.foo()

var e : (() -> A)?
var f = e?()

struct B<T> {}
var g = B<A?>()
