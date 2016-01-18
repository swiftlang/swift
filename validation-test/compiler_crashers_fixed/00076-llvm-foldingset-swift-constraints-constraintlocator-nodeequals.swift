// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

 a
}
struct e : f {
  i f = g
}
func i<g : g, e : f where e.f == g> (c: e) {
}
func i<h : f where h.f == c> (c: h) {
}
i(e())
class a<f : g, g : g where f.f == g> {
}
protocol g {
    typealias f
    typealias e
}
struct c<h : g> : g {
    typealias f = h
    typealias e = a<c<h>, f>
