// RUN: rm -rf %t/clang-module-cache
// RUN: %target-swift-frontend %s -module-cache-path %t/clang-module-cache -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/fluidsonic (Marc Knaup)

import Foundation
class X {
    var x: [String]?
    func a(b: [NSObject: AnyObject]) {
        x = Y().c(b[""])
    }
}
class Y {
    func c(any: Any?) -> [String]? {
        return []
    }
}
