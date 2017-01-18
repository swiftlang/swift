// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
import Foundation
class ed<a>: NSObject {
    var o: a
    y(o: a) {
        r.o = o
        b.y()
    }
}
protocol dc {
    typealias t
    func v(t)
}
struct v<l> : dc {
    func v(v: v.u) {
    }
}
func ^(x: j, s) -> s {
  typealias w
  typealias m = w
  typealias v = w
}
class v<o : k, cb : k n o.dc == cb> : x {
}
class v<o, cb> {
}
protocol k {
    typealias dc
}
class x {
    typealias v = v
}
enum m<a> {
    w cb(a, () -> ())
}
protocol v {
    class func m()
}
struct k {
    var w: v.u
    func m() {
        w.m()
    }
}
protocol dc {
}
struct t : dc {
}
struct cb<k, p: dc n k.cb == p> {
}
struct w<v : m, dc: m n dc.o == v.o> {
}
protocol m {
    q v {
        w k
    }
}
class dc<a : dc
