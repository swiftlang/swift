import _Differentiation

@derivative(of: f)
@_alwaysEmitIntoClient
public func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { 42 * $0 })
}
