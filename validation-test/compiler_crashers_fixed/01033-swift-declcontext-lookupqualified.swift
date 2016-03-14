// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
struct c<d: Sequence, b where Optional<b> == d.Iterator.Element>
