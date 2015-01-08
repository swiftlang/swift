// RUN: %target-swift-frontend %s -emit-sil -verify

// Distributed under the terms of the MIT license
// Test case submitted to project by http://github.com/valfer (Valerio Ferrucci)

protocol C {
    static func c(o: AnyObject) -> Self?
}
func d<A,B>(a : A, f : A -> B) -> B {
} // expected-error {{missing return in a function expected to return 'B'}}
class D<A:C> {
    func e(o: AnyObject) {
        d(o, A.c)
    }
}
