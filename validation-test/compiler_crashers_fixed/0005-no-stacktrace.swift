// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

class c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a) // expected-error {{cannot invoke 'b' with an argument list of type '((Any, AnyObject))'}}
    }
}
