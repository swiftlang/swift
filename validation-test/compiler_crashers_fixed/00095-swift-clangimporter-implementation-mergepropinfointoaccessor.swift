// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
i)
import Foundation
class q<k>: NSObject {
    var j: k
    e ^(l: m, h) -> h {
    f !(l)
}
protocol l {
 d g n()
}
class h: l {
    class g n() { }
}
(h() o l).p.n()
class l<n : h,
