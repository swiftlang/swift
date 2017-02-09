// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol k {
    typealias x
}
class l<ed> {
    init <k: k where k.x == ed>(p: k.x) {
    }
}
extension  k : s {
class ed<n>: p {
    init(i: n) {
   }
}
class k {
    class dc ed() -> r {
        w i fe ed {
        }
    }
}
hg class x
