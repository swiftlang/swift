// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18850296

func f() {
    let x = 0
    class C {
        func f() {
            x
        }
    }
}
