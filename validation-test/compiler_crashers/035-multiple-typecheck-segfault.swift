// RUN: %swift %s -parse -verify
// rdar://17242486
protocol a {
  typealias d // expected-note{{protocol requires nested type 'd'}}
  typealias e = d
  typealias f = d
}
class b<h : c, i : c where h.g == i> : a { // expected-note {{'b' previously declared here}} expected-error{{type 'b<h, i>' does not conform to protocol 'a'}}
}
class b<h, i> { // expected-error {{invalid redeclaration of 'b'}}
}
protocol c {
    typealias g
}