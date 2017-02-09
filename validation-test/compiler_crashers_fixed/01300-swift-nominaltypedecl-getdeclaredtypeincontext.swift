// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
class C<D> {
init <A: A where A.B == D>(e: A.B) {
}
let c = a
func e() {
}
func f<T>() -> T -> T {
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
class a<f : b, g : b where f.d == g> {
}
protocol b {
}
struct c<h : b> : b {
}
struct c<d : Sequence> {
}
func a<d>() -> [c<d>] {
