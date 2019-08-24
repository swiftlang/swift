// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-ir

// REQUIRES: objc_interop

// Issue found by https://github.com/fluidsonic (Marc Knaup)

import Foundation
class X {
    var x: [String]?
    func a(b: [NSObject: AnyObject]) {
        x = Y().c(b["" as NSString])
    }
}
class Y {
    func c(_ any: Any?) -> [String]? {
        return []
    }
}
