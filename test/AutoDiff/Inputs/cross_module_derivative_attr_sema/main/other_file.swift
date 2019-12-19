public func functionDefinedInOtherFile_publicDerivativeInOtherFile(_ x: Float) -> Float { x }

// expected-error @+2 {{a derivative already exists for 'functionDefinedInOtherFile_publicDerivativeInOtherFile'}}
// expected-note @+1 {{other attribute declared here}}
@derivative(of: functionDefinedInOtherFile_publicDerivativeInOtherFile)
public func crossFileDuplicateDerivative1(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}
