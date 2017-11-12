// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
s))
func ^(a: Boolean, Bool) -> Bool {
return !(a)
}
f
e)
func f<g>() -> (g, g -> g) -> g {
d j d.i = {
}
{
g) {
h  }
}
protocol f {
class func i()
}
class d: f{  class func i {}
enum S<T> {
case C(T, () -> ())
}
var x1 = 1
var f1: Int -> Int = {
return $0
}
let succeeds: Int = { (x: Int, f: Int -> Int) -> Int in
return f(x)
}(x1, f1)
let crashes: Int = { x, f in
return f(x)
}(x1, f1)
class c {
func b((Any, c))(a: (Any, AnyObject)) {
b(a)
}
}
protocol a {
typealias d
typealias e = d
typealias f = d
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
typealias g
}
protocol a {
}
protocol b : a {
}
protocol c : a {
}
protocol d {
typealias f = a
}
struct e : d {
typealias f = b
}
func i<j : b, k : d where k.f == j> (n: k) {
}
func i<l : d where l.f == c> (n: l) {
}
i(e())
func c<d {
enum c {
func e
var _ = e
}
}
protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == E> {
}
struct c<d : Sequence> {
var b: d
}
func a<d>() -> [c<d>] {
return []
}
protocol A {
typealias B
func b(B)
}
struct X<Y> : A {
func b(b: X.Type) {
}
}
class a<f : b, g : b where f.d == g> {
}
protocol b {
typealias d
typealias e
pealias e = a<c<h>, d>
}
protocol a {
class func c()
}
class b: a {
class func c() { }
}
(b() as a).dynamicType.c()
func prefix(with: String) -> <T>(() -> T) -> String {
return { g in "\(with): \(g())" }
}
struct A<T> {
let a: [(T, () -> ())] = []
}
func a<T>() {
enum b {
case c
}
}
func a(b: Int = 0) {
}
let c = a
c
