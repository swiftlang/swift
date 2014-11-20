// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func j<d {
struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : A
