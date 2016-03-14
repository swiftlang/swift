// RUN: %target-swift-frontend %s -emit-silgen

// Issue found by https://github.com/fluidsonic (Marc Knaup)

class A {
    var a: () {
        return
    }
    class var a: () {
        return
    }
}
