// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-ir

// Issue found by https://github.com/0xc010d (Ievgen Solodovnykov)

class A {
    init() {
    }
}

extension A {
    convenience init(i: Int) {
        self.init()
    }

    convenience init(s: String) {
        self.init(i: 1)
    }
}
