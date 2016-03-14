// RUN: %target-swift-frontend %s -parse -verify

// Issue found by https://github.com/fluidsonic (Marc Knaup)

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
