// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol b {
func a<d>() -> [c<d>] {
}
var _ = i() {
}
func c<d {
enum c {
}
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
func some<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs : S) -> T? {
for (mx : T?) in xs {
if let x = mx {
}
}
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
var f1: Int -> Int = {
}
let succeeds: Int = { (x: Int, f: Int -> Int) -> Int in
}(x1, f1)
let crashes: Int = { x, f in
}(x1, f1)
func f() {
}
protocol a {
}
class b: a {
}
protocol A {
}
struct X<Y> : A {
func b(b: X.Type) {
}
}
class A<T : A> {
}
class c {
func b((Any, c))(a: (Any, AnyObject)) {
}
}
func a<T>() {
enum b {
}
}
protocol c : b { func b
