// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing
// Distributed under the terms of the MIT license

class a<b : b, d : b where b.d == d> {
}
protocol b {
    typealias d
    typealias e = a<c<h>, d>
}
struct c<d, e: b where d.c == e
