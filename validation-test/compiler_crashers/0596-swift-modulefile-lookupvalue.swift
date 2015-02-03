// RUN: not --crash %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct D : C {
func g<T where T.E == F>(f: B<T>) {
}
}
struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : A>
