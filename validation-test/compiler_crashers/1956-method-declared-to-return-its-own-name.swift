// RUN: not --crash %swift %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/mayoff (Rob Mayoff)

class C {
    func f() -> f {
    }
}
