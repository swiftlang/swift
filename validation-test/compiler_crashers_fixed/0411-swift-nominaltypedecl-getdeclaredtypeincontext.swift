// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
