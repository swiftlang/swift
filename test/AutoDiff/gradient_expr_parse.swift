// RUN: %target-swift-frontend -parse -verify %s

#gradient(foo, withRespectTo:) // expected-error {{use 'wrt:' to specify parameters to differentiate with respect to}}

#gradient(foo) // okay
#gradient(foo, wrt: 1) // expected-error {{expected a parameter, which must be }}
#gradient(foo, wrt: 0) // expected-error {{expected a parameter, which must be }}
#gradient(foo, wrt: .0, self) // expected-error {{expected a parameter, which must be }}
#gradient(foo, wrt: .0, .1) // okay

#valueAndGradient(foo, wrt: .0, .1) // okay

#adjoint(foo(_:_:)) // okay
#adjoint() // expected-error {{expected function name within '#adjoint` expression}}
