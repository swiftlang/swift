public func functionDefinedInModule1_publicDerivativeInModule1(_ x: Float) -> Float { x }

@derivative(of: functionDefinedInModule1_publicDerivativeInModule1)
public func dfModule1(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

public func functionDefinedInModule1_publicDerivativeInModule2(_ x: Float) -> Float { x }
