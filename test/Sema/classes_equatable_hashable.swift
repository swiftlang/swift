// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

class Foo: Equatable {} 
// expected-error@-1 {{type 'Foo' does not conform to protocol 'Equatable'}} expected-note@-1 {{automatic synthesis of 'Equatable' is not supported for classes}}

class Bar: Hashable {} 
// expected-error@-1 {{type 'Bar' does not conform to protocol 'Hashable'}} expected-note@-1 {{automatic synthesis of 'Hashable' is not supported for classes}}
// expected-error@-2 {{type 'Bar' does not conform to protocol 'Equatable'}} expected-note@-2 {{automatic synthesis of 'Equatable' is not supported for classes}}
