// RUN: %target-swift-frontend %s -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

class A {
    var a: () {
        return
    }
    class var a: () {
        return
    }
}
