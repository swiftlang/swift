import module1

@derivative(of: functionDefinedInModule1_publicDerivativeInModule2)
public func dfModule2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}
