// RUN: %target-swift-frontend -typecheck -verify -primary-file %s

class Foo: Equatable {} 
// expected-error@-1 {{type 'Foo' does not conform to protocol 'Equatable'}} 
// expected-note@-2 {{automatic synthesis of 'Equatable' is not supported for class declarations}}
// expected-note@-3 {{add stubs for conformance}}

class Bar: Hashable {} 
// expected-error@-1 {{type 'Bar' does not conform to protocol 'Hashable'}} 
// expected-note@-2 {{automatic synthesis of 'Hashable' is not supported for class declarations}}
// expected-error@-3 {{type 'Bar' does not conform to protocol 'Equatable'}} 
// expected-note@-4 {{automatic synthesis of 'Equatable' is not supported for class declarations}}
// expected-note@-5 {{add stubs for conformance}}
