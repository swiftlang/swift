// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct d<f : e, g: e where g.h == f.h> {{
}
struct B<T : A> {
}
protocol C {
ty    }
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
typealias h
