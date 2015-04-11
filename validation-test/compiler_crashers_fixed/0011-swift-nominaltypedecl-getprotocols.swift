// RUN: %target-swift-frontend %s -parse -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

protocol b {
    static func e()
}
struct c {
    var d: b.Type
    func e() {
        d.e()
    }
}
