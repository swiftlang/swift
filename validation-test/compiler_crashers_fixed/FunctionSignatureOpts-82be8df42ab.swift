// RUN: %target-swift-frontend -O -g -primary-file %s -sil-verify-all -emit-sil

class C {}
enum E<T> {
    case v(T)
}
protocol P {
    associatedtype A
    consuming func f(_ a: consuming A)
}
struct S1<T>: P {
    typealias A = E<T>
    consuming func f(_ a: consuming A) {}
}
struct S2<W: P>: P {
    typealias A = W.A
    let w: W
    init(_ w: consuming W) {
        self.w = w
    }
    consuming func f(_ a: consuming A) {
        self.w.f(a)
    }
}
func g<X: P, Y>(t: consuming X, u: consuming Y) where X.A == Y {
    t.f(u)
}
let c = C()
let e = E<C>.v(c)
let s1 = S1<C>()
let s2 = S2(s1)
g(t: s2, u: e)
