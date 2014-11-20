// RUN: rm -rf %t/clang-module-cache
// RUN: %target-swift-frontend %s -module-cache-path %t/clang-module-cache -emit-ir

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
            println("")
        default:
            println("")
    }
}
