// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %target-swift-frontend %s -module-cache-path %t/clang-module-cache -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://18633758

import Foundation
extension NSFileManager {
    enum C {
    }
    class D {
        class var a: Dictionary<String, C> {
            struct E {
                static var e = Dictionary<String, C>()
            }
            return E.e
        }
    }
}
