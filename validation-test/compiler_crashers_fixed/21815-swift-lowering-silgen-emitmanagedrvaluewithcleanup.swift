// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

struct S<T> {
    init(f: AutoreleasingUnsafeMutablePointer<T?> -> ()) {
        var t: T?
        f(&t)
    }
}

