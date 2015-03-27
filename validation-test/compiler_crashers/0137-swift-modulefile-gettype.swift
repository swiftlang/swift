// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct w<p : x> {
}
func t<p>() -> [w<p>] {
}
struct p<l : u, w: u where w.j == l.j> {
}
protocol u {
}
class u<v : u> {
}
class t<l : b, w : b where l.p == w> {
}
struct w<j : b> : b {
}
protocol u {
}
struct m<v : u> {
}
protocol l {
}
struct w : l {
}
struct b<j> : u {
    func b(b: b.s) {
    }
}
protocol t {
}
class b<j : w, u : w where j.w == u> : t {
}
class b<j, u> {
}
protocol w {
}
class t {
}
protocol t : t
