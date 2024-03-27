// REQUIRES: differentiable_programming
// RUN: %target-swift-frontend -emit-ir -verify %s

// This used to trigger an assertion due to LoadableByAddress incorrectly doing incorrect transforms
// around linear map tuple users (pointer_to_address).

import _Differentiation
struct H: Differentiable {}
protocol J: Differentiable {}
struct L: Differentiable {
    var p: [P]
    @differentiable(reverse) func s() -> H {
        var m = 0.0
        for i in 0 ..< withoutDerivative(at: p.count) {
            m += p[i].a
            m += p[i].a
            m += p[i].a
            m += p[i].a
        }
        return P.g(p: P(a: 0.0, b: 0.0, c: 0.0, d: m), z: L( p: self.p)).w
    }
}
struct P: J {
    var a = 0.0
    var b = 0.0
    var c = 0.0
    var d = 0.0
    var e = 0.0
    @differentiable(reverse) static func g(p: P, z: L) -> Y<P> {return Y<P>(w: H())}
}
struct Y<U: J>: Differentiable {var w: H = H()}
