// RUN: %target-typecheck-verify-swift -enable-parameterized-existential-types
//
// FIXME: Merge this file with existential_metatypes.swift once -enable-parameterized-existential-types becomes the default

protocol P<T> {
  associatedtype T
}

protocol Q<T> {
  associatedtype T
}

protocol PP<U>: P<Self.U.T> {
  associatedtype U: P<U>
}

var qp: (any Q<Int>).Type
var pp: (any P<Int>).Type = qp // expected-error{{cannot convert value of type '(any Q<Int>).Type' to specified type '(any P<Int>).Type'}}

var qt: any Q<Int>.Type
qt = qp // expected-error{{cannot assign value of type '(any Q<Int>).Type' to type 'any Q<Int>.Type'}}
qp = qt // expected-error{{cannot assign value of type 'any Q<Int>.Type' to type '(any Q<Int>).Type'}}
var pt: any P<Int>.Type = qt // expected-error{{cannot convert value of type 'any Q<Int>.Type' to specified type 'any P<Int>.Type'}}
pt = pp // expected-error{{cannot assign value of type '(any P<Int>).Type' to type 'any P<Int>.Type'}}
pp = pt // expected-error{{cannot assign value of type 'any P<Int>.Type' to type '(any P<Int>).Type'}}

var ppp: (any PP<Int>).Type
pp = ppp // expected-error{{cannot assign value of type '(any PP<Int>).Type' to type '(any P<Int>).Type'}}

var ppt: any PP<Int>.Type
pt = ppt
