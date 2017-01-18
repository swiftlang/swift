// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
 a
}
struct e : f {
  i f = g
}
func i<g : g, e : f where e.f == g> (c: e) {
}
func i<h : f where h.f == c> (c: h) {
}
i(e())
class a<f : g, g : g where f.f == g> {
}
protocol g {
    typealias f
    typealias e
}
struct c<h : g> : g {
    typealias f = h
    typealias e = a<c<h>, f>
