// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol k {
typealias m
}
struct e<j : k> {n: j
let i: j.m
}
func g<f>() -> (f, f -> f) -> f {
g e {}
struct g<g where f.b ==g.b
