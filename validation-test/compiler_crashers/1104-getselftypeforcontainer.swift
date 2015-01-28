// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts
// XFAIL: asan

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct l<l : d> : d {
i j i() {
}
}
protocol f {
}
protocol d : f {
struct c<d : SequenceType> {
}
func a<d>() -> [c<d>] {
}
func a<T>() -> (T, T -> T) -> T {
}
func r<t>() {
f f {
}
}
struct i<o : u> {
}
func r<o>() -> [i<o>] {
}
class g<t : g> {
}
class g: g {
}
class n : h {
}
protocol g {
func i() -> l  func o() -> m {
}
}
func j<t k t: g, t: n>(s: t) {
}
protocol r {
}
protocol f : r {
