// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol f : f {
}
func h<d {
    enum h {
        func e
        var _ = e
    }
}
protocol e {
    e func e()
}
struct h {
    var d: e.h
    func e() {
        d.e()
    }
}
protocol f {
  i []
}
func f<g>() -> (g, g -> g) -> g
