import _Differentiation

@inlinable
@derivative(of: min)
func minVJP<T: Comparable & Differentiable>(
    _ x: T,
    _ y: T
) -> (value: T, pullback: (T.TangentVector) -> (T.TangentVector, T.TangentVector)) {
    func pullback(_ v: T.TangentVector) -> (T.TangentVector, T.TangentVector) {
        if x <= y {
            return (v, .zero)
        }
        else {
            return (.zero, v)
        }
    }
    return (value: min(x, y), pullback: pullback)
}

extension Struct {
    @inlinable
    @derivative(of: max) // expected-error {{cannot find 'max' in scope}}
    static func maxVJP<T: Comparable & Differentiable>(
        _ x: T,
        _ y: T
    ) -> (value: T, pullback: (T.TangentVector) -> (T.TangentVector, T.TangentVector)) {
        func pullback(_ v: T.TangentVector) -> (T.TangentVector, T.TangentVector) {
            if x < y {
                return (.zero, v)
            }
            else {
                return (v, .zero)
            }
        }
        return (value: max(x, y), pullback: pullback)
    }
}
