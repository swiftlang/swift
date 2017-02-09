// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func i(c: () -> ()) {
}
class a {
var _ = i() {
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
struct A<T> {
}
protocol b {
}
struct c {
}
var e: Int -> Int = {
}
let d: Int =  { c, b in
}(f, e)
struct d<f : e, g: e where g.h == f.h> {
}
func g<T== F>(f: B<T>) {
}
}
func a() {
for c in 0..<d) {
}
.b =}
}
import Foundation
