// RUN: %target-swift-frontend -typecheck -verify %S/Inputs/differentiable_attr_type_checking_non_primary_file.swift -primary-file %s

// Test TF-1043: Type-checking protocol requirement `@differentiable` attributes
// from non-primary files.

struct OuterLayer: Layer {
  typealias Input = Float
  typealias Output = Float

  var dummy: DummyLayer

  @differentiable
  var computedProperty: Output {
    // NOTE(TF-1043): Old misleading error:
    // error: 'Int' is not convertible to 'Float'
    // return Float(1).sequenced(through: dummy)
    //        ^~~~~~~~
    return Float(1).sequenced(through: dummy)
  }

  @differentiable
  func instanceMethod(_ input: Input) -> Output {
    // NOTE(TF-1043): Old misleading error:
    // error: type of expression is ambiguous without more context
    // return input.sequenced(through: dummy)
    //        ~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~
    return input.sequenced(through: dummy)
  }
}
