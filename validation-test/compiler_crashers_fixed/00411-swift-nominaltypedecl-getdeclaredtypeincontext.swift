// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct c {
var d: b.Type
f<T> {
}
func a<T>() -> (T, T -> T) -> T {
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
protocol A {
}
class B {
func d() -> String {
}
}
class C: B, A {
override func d() -> String {
}
func c() -> String {
}
}
func e<T where T: A, T: B>(t: T) {
}
class c {
func b((Any, c))(a: (Any, AnyObject)) {
}
}
func i(c: () -> ()) {
}
class a {
var _ = i() {
}
}
func b(c) -> <d>(() -> d) {
}
class A<T : A> {
}
struct c<d, e: b where d.c == e> {
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
class b<h : c, i : c where h.g == i> : a {
