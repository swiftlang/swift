// RUN: not %target-swift-frontend %s

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://18678220

class A {
    func a() -> Int {
        return 0
    }
}
let B : A = {
    class C : A {
        private override func a() -> Int {
            return 0
        }
    }
    return C()
}()
