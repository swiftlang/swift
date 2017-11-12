// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s

// Issue found by https://github.com/robrix (Rob Rix)
// http://www.openradar.me/18332184
// https://twitter.com/rob_rix/status/510875744667701248

if true {
    struct S {
        let v: Int
    }
    let g = {
        S(v: $0)
    }
    let f = {
        0
    }
    func compose<T, U, V>(g: U -> V, f: T -> U)(x: T) -> V {
        return g(f(x))
    }
    let h = compose(g, f)
}
