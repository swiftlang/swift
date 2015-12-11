// RUN: not --crash %target-swift-frontend %s -emit-silgen
// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/airspeedswift (airspeedswift)

struct S<T> {
    var a: [T] = []
}
extension S {
    init(other: [T]) {
        a = other
    }
}
