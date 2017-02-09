// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct w<p : x> {
}
func t<p>() -> [w<p>] {
}
struct p<l : u, w: u where w.j == l.j> {
}
protocol u {
}
class u<v : u> {
}
class t<l : b, w : b where l.p == w> {
}
struct w<j : b> : b {
}
protocol u {
}
struct m<v : u> {
}
protocol l {
}
struct w : l {
}
struct b<j> : u {
    func b(b: b.s) {
    }
}
protocol t {
}
class b<j : w, u : w where j.w == u> : t {
}
class b<j, u> {
}
protocol w {
}
class t {
}
protocol t : t
