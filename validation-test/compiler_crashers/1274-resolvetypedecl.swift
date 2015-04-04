// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func b(z: (((Any, Any) -> Any) -> Any)) -> Any {
return z({
(p: Any, q:Any) -> Any in
nType, Bool) -> Bool {
}
protocol A {
}
class C<D> {
init <A: A where A.B == D>(e: A.B) {
}
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
}
func b(c) -> <d>(() -> d) {
}
protocol A {
}
struct X<Y> : A {
func b(b: X.Type) {
}
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
}
struct c<d, e: b where d.c == e> {
