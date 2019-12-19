import module1
import module2

// Check that we reject derivatives with incorrect access level.

// expected-note @+1 {{original function defined here}}
func internalFunctionWithoutDifferentiableWithPublicDerivative(_ x: Float) -> Float { x }

// expected-error @+1 {{derivative function visibility must be at least as restrictive as original function visibility}}
@derivative(of: internalFunctionWithoutDifferentiableWithPublicDerivative)
public func dInternalFunctionWithoutDifferentiableWithPublicDerivative(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

@differentiable
// expected-note @+1 {{original function defined here}}
func internalFunctionWithDifferentiableWithPublicDerivative(_ x: Float) -> Float { x }

// expected-error @+1 {{derivative function visibility must be at least as restrictive as original function visibility}}
@derivative(of: internalFunctionWithDifferentiableWithPublicDerivative)
public func dInternalFunctionWithDifferentiableWithPublicDerivative(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

// TODO(TF-1071): Reject derivatives with differing access levels.
public func derivativesHaveDifferentAccessLevels(_ x: Float) -> Float { x }

@derivative(of: derivativesHaveDifferentAccessLevels)
public func dDerivativesHaveDifferentAccessLevels(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
  fatalError()
}

@derivative(of: derivativesHaveDifferentAccessLevels)
func dDerivativesHaveDifferentAccessLevels2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

// Check that cross-file and cross-module duplicate derivatives are rejected.

// expected-error @+2 {{a derivative already exists for 'functionDefinedInOtherFile_publicDerivativeInOtherFile'}}
// expected-note @+1 {{other attribute declared here}}
@derivative(of: functionDefinedInOtherFile_publicDerivativeInOtherFile)
public func crossFileDuplicateDerivative2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

// TODO(TF-1070): There should be a duplicate derivative error here.
@derivative(of: functionDefinedInModule1_publicDerivativeInModule1)
public func crossModuleDuplicateDerivative1(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}

// TODO(TF-1070): There should be a duplicate derivative error here.
@derivative(of: functionDefinedInModule1_publicDerivativeInModule2)
public func crossModuleDuplicateDerivative2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}
