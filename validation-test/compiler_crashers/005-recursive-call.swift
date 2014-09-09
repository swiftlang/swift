// RUN: %swift %s -parse -verify
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

class c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a) // expected-error{{type of expression is ambiguous without more context}}
    }
}
