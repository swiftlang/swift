import _Differentiation

@_alwaysEmitIntoClient
public func f(_ x: Float) -> Float {
  x
}

@derivative(of: f)
@_alwaysEmitIntoClient
public func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  (x, { 42 * $0 })
}
