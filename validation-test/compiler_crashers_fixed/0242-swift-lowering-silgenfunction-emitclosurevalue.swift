// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> String {
    }
    class func b() { // expected-note {{'self' declared here}}
        struct c { // expected-note {{type declared here}}
            static let d: String = {
                return self.a() // expected-error {{struct declaration cannot close over value 'self' defined in outer scope}}
            }()
        }
    }
}
