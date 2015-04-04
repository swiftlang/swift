// RUN: not --crash %target-swift-frontend %s -parse
// REQUIRES: asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func s() -> o {
        r q {
}
protocol p {
  typealias m = q
}
m r : p {
}
func r<s : t, o : p where o.m == s> (s: o) {
}
func r<s : p where s.m == s> (s: s) {
}
m s<p : u> {
}
class t<s : s, r : s where s.t == r> : q {
}
class t<s, r> {
}
protocol s {
    func r() {
    }
}
func t(s) -> <p>(() -> p) {
}
class q<m : t
