// RUN: %target-swift-frontend -typecheck %s -verify -verify-ignore-unknown

class Foo: Codable { 
    // expected-error@-1 2{{type 'Foo' does not conform to protocol 'Decodable'}}
    let foo = Foo() 
    // expected-error@-1 2{{circular reference}}
    // expected-note@-2 4{{through reference here}}
    // expected-note@-3 {{cannot automatically synthesize 'Decodable' because '<<error type>>' does not conform to 'Decodable'}}
    // expected-note@-4 {{cannot automatically synthesize 'Decodable' because 'Foo' does not conform to 'Decodable'}}
}
