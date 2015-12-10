// RUN: %target-swift-frontend %s -emit-silgen
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/19423783

// This test crashes on Linux.
// XFAIL: linux

protocol A {
    var a: Int {
        get
    }
}
class B: A {
    let a = 42
}
let b = B()
let c = [b]
let d = c as [A]
