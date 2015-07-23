// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a<f : g, g : g where f.f == g> {
}
protocol g {
typealias f
}
struct c<h : g> : g {
typealias e = a<c<h>, f>
class d<c>: NSObject {
init(b: c) {
g) {
h  }
}
protocol f {
}}
struct c<d: SequenceType, b where Optional<b> == d.Generator.Element>
