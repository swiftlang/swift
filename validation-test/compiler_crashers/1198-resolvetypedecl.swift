// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
return {
}
struct X<Y> : A {
func b(b: X.Type) {
}
}
class d<c>: NSObject {
init(b: c) {
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
protocol a : a {
}
class A : A {
}
class B : C {
}
class c {
func b((Any, c))(a: (Any, AnyObject)) {
}
}
protocol b {
}
struct c {
func e() {
}
}
func d<b: SequenceType, e where Optional<e> == b.Generat<d>(() -> d) {
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
enum S<T> : P {
func f<T>() -> T -> T {
}
}
protocol P {
}
func a(b: Int = 0) {
}
struct c<d, e: b where d.c == e
