// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
struct c<d : SequenceType> {
}
func a<d>() -> [c<d>] {
