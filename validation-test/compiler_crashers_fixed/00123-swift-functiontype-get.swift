// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func g<h>() -> (h, h -> h) -> h {
    f f: ((h, h -> h) -> h)!
    j f
}
protocol f {
    class func j()
}
struct i {
    f d: f.i
    func j() {
        d.j()
    }
}
class g {
    typealias f = f
}
func g(f: Int = k) {
}
let i = g
