// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
struct c<d : SequenceType> {
}
func a<d>() -> [c<d>] {
