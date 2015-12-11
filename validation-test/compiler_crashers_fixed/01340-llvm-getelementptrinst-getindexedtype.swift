// RUN: %target-swift-frontend %s -emit-ir

// REQUIRES: objc_interop

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
