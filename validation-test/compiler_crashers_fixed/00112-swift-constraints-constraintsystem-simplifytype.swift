// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse
o
}
class f<p : k, p : k where p.n == p> : n {
}
class f<p, p> {
}
protocol k {
    typealias n
}
o: i where k.j == f> {l func k() { }
}
(f() as n).m.k()
func k<o {
    enum k {
        func o
        var _ = o
