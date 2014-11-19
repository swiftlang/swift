// RUN: not --crash %swift %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

class A {
    class func a() -> String {
    }
    class func b() {
        struct c {
            static let d: String = {
                return self.a()
            }()
        }
    }
}
