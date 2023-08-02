// RUN: %target-swift-frontend -emit-sil -verify %s

import _Differentiation

// expected-error @+1 {{function is not differentiable}}
@differentiable(reverse)
// expected-note @+1 {{when differentiating this function definition}}
func o(ff: F) -> Double {
    var y = ff.i?.first { $0 >= 0.0 } ?? 0.0
    while 0.0 < y {
        // expected-note @+1 {{expression is not differentiable}}
	y = ff.g() ?? y
    }
    return y
}

public struct F: Differentiable {
    @noDerivative var i: [Double]? {return nil}
    func g() -> Double? {return nil}
}
