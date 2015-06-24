// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

class c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a)
    }
}
