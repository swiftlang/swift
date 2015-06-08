// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> Self {
        return b(self.dynamicType) // expected-error {{cannot invoke 'b' with an argument list of type '(Self.Type.Type)'}}
        // expected-note@-1{{expected an argument list of type '(AnyObject.Type)'}}
    }
}
func b<T>(t: AnyObject.Type) -> T! {
    return nil
}
