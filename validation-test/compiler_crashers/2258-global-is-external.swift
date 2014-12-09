// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// https://twitter.com/TheRealBnut/status/533901920335720449

class A {
}
var t : A = A()
_ = {
    [weak t]()->A in {
        [weak t] ()->A in t!
    }()
}()
