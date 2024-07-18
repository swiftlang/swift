// RUN: %target-swift-frontend -emit-sil -verify %s

import _Differentiation

// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func o(ff: F) -> Double {
    var y = ff.i?.first { $0 >= 0.0 } ?? 0.0
    while 0.0 < y {
        // This one is not differentiable since rhs of ?? is an autoclosure that we cannot differentiate wrt.
        // The variant below has rhs as non-active and therefore everything works.
        // expected-note @+1 {{cannot differentiate through a non-differentiable argument; do you want to use 'withoutDerivative(at:)'}}
	y = ff.g() ?? y
    }
    return y
}

func o2(ff: F) -> Double {
    var y = ff.i?.first { $0 >= 0.0 } ?? 0.0
    while 0.0 < y {
	y = ff.g() ?? 42
    }
    return y
}

public struct F: Differentiable {
    @noDerivative var i: [Double]? {return nil}
    func g() -> Double? {return nil}
}
