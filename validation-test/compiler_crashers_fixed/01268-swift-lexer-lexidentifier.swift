// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
protocol k {
typealias m
}
struct e<j : k> {n: j
let i: j.m
}
func g<f>() -> (f, f -> f) -> f {
g e {}
struct g<g where f.b ==g.b
