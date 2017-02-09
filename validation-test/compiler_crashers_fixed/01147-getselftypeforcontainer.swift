// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
class d: f{  class func i {}
protocol A {
}
protocol A {
}
class C<D> {
init <A: A where A.B == D>(e: A.B) {
}
}
: a {
}
protocol c : a {
}
protocol d {
typealias f = a
}
struct e : d {
}
func i<j : b, k : d where k.f == j> (n: k) {
}
func i<l : d where l.f = 0) {
}
let c = a
protocol c : b { func b
protocol a {
ss b<h : c, i : c where }
}
struct c<d : Sequence> {
}
func a<d>() -> [c<d>] {
