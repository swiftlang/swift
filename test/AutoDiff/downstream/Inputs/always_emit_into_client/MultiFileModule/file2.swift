@derivative(of: f)
@_alwaysEmitIntoClient
public func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { 10 * $0 })
}
