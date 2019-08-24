// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct e : d {
class a<f : b, g : b where f.d == g> {
}
protocol b {
}
struct c<h : b> : b {
}
func a(b: Int = 0) {
}
let c = a
func a<T>() {
enum b {
}
}
struct A<T> {
}
func d<b: Sequence, e where Optional<e> == b.Iterator.Element>(c : b) -> e? {
for (mx : e?) in c {
}
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
protocol a : a {
}
class A : A {
}
class B : C {
}
protocol A {
}
struct B<T : A> {
}
protocol C {
}
struct D : C {
func g<T where T.E == F>(f: B<T>) {
}
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a
