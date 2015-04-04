// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: asan
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
func d<b: SequenceType, e where Optional<e> == b.Generator.Element>(c : b) -> e? {
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
