// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func d(e: () -> ()) {}
class e {
    var _ = d() {
class b<g : e, d : e where g.f == d> : e
