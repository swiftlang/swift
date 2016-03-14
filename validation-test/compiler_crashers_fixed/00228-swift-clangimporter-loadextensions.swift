// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
