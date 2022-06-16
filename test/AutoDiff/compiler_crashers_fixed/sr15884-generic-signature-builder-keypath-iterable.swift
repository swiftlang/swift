// RUN: %target-swift-frontend -emit-sil -verify %s

// SR-15884: Crash while compiling Swift for TensorFlow. This was caused by a
// bug in GenericSignatureBuilder, similar to one involving `CaseIterable`. In
// this reproducer, `KeyPathIterabe` is similar enough to `CaseIterable` to 
// cause the GSB crash. It was fixed by RequirementMachine abstract signatures.

import _Differentiation
@_spi(Reflection) import Swift

struct RNNCellInput<Input>: Differentiable {}
struct RNNCellOutput<Output>: Differentiable {}

protocol Layer: Differentiable {
  associatedtype Input
  associatedtype Output: Differentiable
  
  @differentiable(reverse)
  func callAsFunction(_ input: Input) -> Output
}

public protocol KeyPathIterable {
  associatedtype AllKeyPaths: Sequence
    where AllKeyPaths.Element == PartialKeyPath<Self>
}

protocol RecurrentLayerCell: Layer, KeyPathIterable
where
  Input == RNNCellInput<TimeStepInput>,
  Output == RNNCellOutput<TimeStepOutput>
{
  associatedtype TimeStepInput
  associatedtype TimeStepOutput: Differentiable
}

struct RecurrentLayer<Cell: RecurrentLayerCell>: Layer {
  typealias Input = Cell.TimeStepInput
  typealias Output = Cell.TimeStepOutput

  var cell: Cell

  @differentiable(reverse)
  func callAsFunction(_ inputs: Cell.TimeStepInput) -> Cell.TimeStepOutput {
    // expected-warning @+1 {{function call causes an infinite recursion}}
    return self(inputs)
  }
}
