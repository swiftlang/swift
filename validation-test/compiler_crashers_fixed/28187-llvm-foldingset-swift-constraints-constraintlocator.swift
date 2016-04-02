// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
// REQUIRES: objc_interop

// Test case found by https://github.com/allu22 (Alvar Hansen)

class A {
    func a() {
        A().d { [weak self] (c) -> Void in
            self?.b(c)
        }
    }
    func d(e: ((AnyObject)->Void)) {
    }
    func b(f: AnyObject, g: AnyObject) {
    }
}
