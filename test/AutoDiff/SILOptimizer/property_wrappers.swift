// RUN: %target-swift-frontend -emit-sil -verify %s %S/Inputs/nontrivial_loadable_type.swift
// REQUIRES: asserts

// Test property wrapper differentiation coverage for a variety of property
// types: trivial, non-trivial loadable, and address-only.

import DifferentiationUnittest

// MARK: Property wrappers

@propertyWrapper
struct SimpleWrapper<Value> {
  var wrappedValue: Value // stored property
}

@propertyWrapper
struct Wrapper<Value> {
  private var value: Value
  var wrappedValue: Value { // computed property
    get { value }
    set { value = newValue }
  }

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }
}

// `DifferentiableWrapper` conditionally conforms to `Differentiable`.
@propertyWrapper
struct DifferentiableWrapper<Value> {
  private var value: Value
  var wrappedValue: Value { // computed property
    get { value }
    set { value = newValue }
  }

  init(wrappedValue: Value) {
    self.value = wrappedValue
  }
}
extension DifferentiableWrapper: Differentiable where Value: Differentiable {}

// MARK: Types with wrapped properties

struct Struct: Differentiable {
  @Wrapper @SimpleWrapper var trivial: Float = 10
  @Wrapper @SimpleWrapper var tracked: Tracked<Float> = 20
  @Wrapper @SimpleWrapper var nontrivial: NontrivialLoadable<Float> = 30
  // Tests SR-12800: semantic member accessors should have empty linear map structs.
  @DifferentiableWrapper var differentiableWrapped: Float = 40

  static func testGetters() {
    let _: @differentiable (Self) -> Float = { $0.trivial }
    let _: @differentiable (Self) -> Tracked<Float> = { $0.tracked }
    let _: @differentiable (Self) -> NontrivialLoadable<Float> = { $0.nontrivial }
    let _: @differentiable (Self) -> Float = { $0.differentiableWrapped }
  }

  static func testSetters() {
    let _: @differentiable (inout Self, Float) -> Void =
      { $0.trivial = $1 }
    let _: @differentiable (inout Self, Tracked<Float>) -> Void =
      { $0.tracked = $1 }
    let _: @differentiable (inout Self, NontrivialLoadable<Float>) -> Void =
      { $0.nontrivial = $1 }
    let _: @differentiable (inout Self, Float) -> Void =
      { $0.differentiableWrapped = $1 }
  }
}

struct GenericStruct<T: Differentiable>: Differentiable {
  @Wrapper @SimpleWrapper var trivial: Float = 10
  @Wrapper @SimpleWrapper var tracked: Tracked<Float> = 20
  @Wrapper @SimpleWrapper var nontrivial: NontrivialLoadable<Float> = 30
  @Wrapper @SimpleWrapper var addressOnly: T

  // SR-12778: Test getter pullback for non-trivial loadable property.
  static func testGetters() {
    let _: @differentiable (Self) -> Float = { $0.trivial }
    let _: @differentiable (Self) -> Tracked<Float> = { $0.tracked }
    let _: @differentiable (Self) -> NontrivialLoadable<Float> = { $0.nontrivial }
    let _: @differentiable (Self) -> T = { $0.addressOnly }
  }

  // SR-12779: Test setter pullback for non-trivial loadable property.
  static func testSetters() {
    let _: @differentiable (inout Self, Float) -> Void =
      { $0.trivial = $1 }
    let _: @differentiable (inout Self, Tracked<Float>) -> Void =
      { $0.tracked = $1 }
    let _: @differentiable (inout Self, NontrivialLoadable<Float>) -> Void =
      { $0.nontrivial = $1 }
    let _: @differentiable (inout Self, T) -> Void =
      { $0.addressOnly = $1 }
  }
}
