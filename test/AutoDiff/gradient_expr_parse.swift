// RUN: %target-swift-frontend -parse -verify %s

#gradient(foo, wrt:) // expected-error {{expected label 'of:'}}
#gradient(of: foo, wrt:) // expected-error {{expected label 'withRespectTo:'}}

#gradient(of: foo) // okay
#gradient(of: foo, withRespectTo: 1) // expected-error {{expected an argument, which can be }}
#gradient(of: foo, withRespectTo: 0) // expected-error {{expected an argument, which can be }}
#gradient(of: foo, withRespectTo: .0) // okay
#gradient(of: foo, withRespectTo: .0, .1, self) // okay

#valueAndGradient(foo, wrt:) // expected-error {{expected label 'of:'}}
#valueAndGradient(of: foo, withRespectTo: .0, .1) // okay
