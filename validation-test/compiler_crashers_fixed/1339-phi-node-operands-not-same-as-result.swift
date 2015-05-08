// RUN: %target-swift-frontend %s -emit-ir

// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// http://www.openradar.me/18756378

import Foundation
enum A<T : AnyObject> {
    case E(T)
}
class B {
}
func c(d : A<B>) {
    switch (d) {
        case .E(var e):
            print("")
        default:
            print("")
    }
}
