// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func r<t>() {
    f f {
        i i
    }
}
struct i<o : u> {
    o f: o
}
func r<o>() -> [i<o>] {
    p []
}
class g<t : g> {
}
class g: g {
}
class n : h {
}
typealias h = n
protocol g {
    func i() -> l  func o() -> m {
        q""
    }
}
func j<t k t: g, t: n>(s: t) {
    s.i()
}
protocol r {
}
protocol f : r {
}
protocol i : r {
}
j
