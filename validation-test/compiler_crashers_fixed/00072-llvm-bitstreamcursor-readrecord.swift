// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
class f<d : d, j : d k d.l == j> {
}
protocol d {
    i l
    i i
}
struct l<l : d> : d {
    i j i() {
        l.i()
    }
}
protocol f {
}
protocol d : f {
