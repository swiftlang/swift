//===-- Tensor.swift ------------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This is the core Tensor abstraction, which is conceptually equivalent to a
// NumPy ndarray.  It carries no rank information in its static type, so it can
// be used by model developers who don't want it.
//
//===----------------------------------------------------------------------===//

// NOTE: Pretty much everything here is marked @_inlineable/@_versioned.  This
// causes the body to be serialized into the generated Swift module, which
// allows it to be inlined into the user's code during deabstraction.  This is
// really gross, and will get better one way or the other, through compiler
// changes.

import Swift

// Python PEP 465 makes a compelling argument that matrix multiplication should
// not be spelled with the standard * operator, so we need a new one.  We'll use
// the operator, though it is defensible to use a variety of other ones as well.
infix operator • : MultiplicationPrecedence

public struct Tensor<Element : TensorElementProtocol> {
  // State owned by this tensor.
  @_versioned
  internal // FIXME: Should be fileprivate, but Swift doesn't allow that yet.
  let core : TensorCore<Element>

  @_versioned
  @_inlineable
  internal init(_ c : TensorCore<Element>) {
    core = c
  }

  @_inlineable
  public var rank : Int { return tfop_rank(core) }
  @_inlineable
  public var shape : [Int] { return tfop_shape(core) }

  /// Indicate that this tensor is being moved to the accelerator.
  @_inlineable
  public func toDevice() -> Tensor<Element> { return Tensor(tfop_send(core)) }

  /// Indicate that this tensor is being moved to the host.
  @_inlineable
  public func toHost() -> Tensor<Element> { return Tensor(tfop_receive(core)) }
}

// Initializers
public extension Tensor {
  /// Perform an element conversion from Tensor<U> to Tensor<T>.
  @_inlineable
  public init<FromType>(_ t : Tensor<FromType>) {
    core = tfop_elt_convert(t.core)
  }

  // zeroD initializer, takes exactly one value.
  @_inlineable
  init(zeroD value: Element) {
    self.init(tfop_literal_0d(value))
  }

  // oneD initializer, takes an array of values.
  @_inlineable
  init(oneD values: [Element]) {
    self.init(tfop_literal_1d(values))
  }

  // oneD initializer, takes a vararg list of values.
  @_inlineable
  init(oneD values: Element...) {
    self.init(oneD: values)
  }

  @_inlineable
  init(twoD values: [[Element]]) {
    self.init(tfop_literal_2d(values))
  }

  @_inlineable
  init(twoD values: [Element]...) {
    self.init(twoD: values)
  }

  /// Zero initializer, takes a list of dimensions.
  @_inlineable
  init(zeros dimensions: Int...) {
    self.init(zeros: dimensions)
  }
  @inline(never)  // FIXME: Remove this.
  init(zeros dimensions: [Int]) {
    self.init(dimensions.withUnsafeBufferPointer { dimensions in
      tfop_init_zeros(dimensions)
    })
  }
  /// Ones initializer, takes a list of dimensions.
  @_inlineable
  init(ones dimensions: Int...) {
    self.init(ones: dimensions)
  }
  @inline(never)  // FIXME: remove this.
  init(ones dimensions: [Int]) {
    self.init(dimensions.withUnsafeBufferPointer { dimensions in
      tfop_init_ones(dimensions)
    })
  }

  // def tf.random_normal(shape, mean=0.0, stddev=1.0, dtype=dtypes.float32,
  //                      seed=None, name=None):
  @inline(never)  // FIXME: Remove this.
  public init(randomNormal dimensions: Int...,
    mean: Double = 0, stddev: Double = 1) {
    self.init(tfop_init_random_normal(dimensions, mean, stddev))
  }

  // def tf.eye(shape)
  @inline(never)  // FIXME: Remove this.
  public init(eye dimensions: [Int]) {
    self.init(tfop_init_eye(dimensions))
  }
}

// Subscripting a tensor produces a smaller tensor.
public extension Tensor {
  @_inlineable
  subscript(tensor indices : Int...) -> Tensor {
    return Tensor(tfop_subscript_tensor(core, indices))
  }

  // Subscript to get a scalar.
  @_inlineable
  subscript(scalar indices : Int...) -> Element {
    return tfop_subscript_scalar(core, indices)
  }

  // Slicing out a range of elements.
  // TODO: begin/end are vectors in general.
  // tfop_slice(tensor, begin, end) -> tensor
  @_inlineable
  subscript(slice : Range<Int>) -> Tensor {
    return Tensor(tfop_slice(core, start: slice.lowerBound,
                             end: slice.upperBound))
  }
}

extension TensorElementProtocol {
  // Scalar to Tensor broadcast
  @_versioned @_inlineable
  func broadcastTo(rank: Int) -> Tensor<Self> {
    return Tensor(tfop_broadcast_scalar(self, rank))
  }
}
public extension Tensor {
  // Tensor to Tensor broadcast
  @_inlineable
  func broadcastTo(rank: Int) -> Tensor {
    return Tensor(tfop_broadcast(core, rank))
  }

  @_inlineable
  func broadcastTo(tensor: Tensor) -> Tensor {
    return Tensor(tfop_broadcast_to(core, tensor.core))
  }
}


/// Arithmetic Operators.
public extension Tensor {
  @_inlineable
  static func +(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(tfop_elt_add(lhs.core, rhs.core))
  }
  @_inlineable
  static func +(lhs: Tensor, rhs: Element) -> Tensor {
    return lhs + rhs.broadcastTo(rank: lhs.rank)
  }
  @_inlineable
  static func +(lhs: Element, rhs: Tensor) -> Tensor {
    return lhs.broadcastTo(rank: rhs.rank) + rhs
  }
  @_inlineable
  static func +=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs + rhs
  }

  @_inlineable
  static func -(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(tfop_elt_subtract(lhs.core, rhs.core))
  }
  @_inlineable
  static func -(lhs: Tensor, rhs: Element) -> Tensor {
    return lhs - rhs.broadcastTo(rank: lhs.rank)
  }
  @_inlineable
  static func -(lhs: Element, rhs: Tensor) -> Tensor {
    return lhs.broadcastTo(rank: rhs.rank) - rhs
  }
  @_inlineable
  static func -=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs - rhs
  }
  @_inlineable
  static func -=(lhs: inout Tensor, rhs: Element) {
    lhs = lhs - rhs
  }

  @_inlineable
  static func *(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(tfop_elt_multiply(lhs.core, rhs.core))
  }
  @_inlineable
  static func *(lhs: Element, rhs: Tensor) -> Tensor {
    return lhs.broadcastTo(rank: rhs.rank) * rhs
  }
  @_inlineable
  static func *(lhs: Tensor, rhs: Element) -> Tensor {
    return lhs * rhs.broadcastTo(rank: lhs.rank)
  }

  @_inlineable
  static func /(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(tfop_elt_divide(lhs.core, rhs.core))
  }
  @_inlineable
  static func /(lhs: Tensor, rhs: Element) -> Tensor {
    return lhs / rhs.broadcastTo(rank: lhs.rank)
  }
  @_inlineable
  static func /(lhs: Element, rhs: Tensor) -> Tensor {
    return lhs.broadcastTo(rank: rhs.rank) / rhs
  }
  @_inlineable
  static func /=(lhs: inout Tensor, rhs: Tensor) {
    lhs = lhs / rhs
  }
  @_inlineable
  static func /=(lhs: inout Tensor, rhs: Element) {
    lhs = lhs / rhs
  }


  @_inlineable
  static func •(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(tfop_elt_matmul(lhs.core, rhs.core))
  }

  @_inlineable
  static func •(lhs: Element, rhs: Tensor) -> Tensor {
    return lhs.broadcastTo(rank: rhs.rank) • rhs
  }
  @_inlineable
  static func •(lhs: Tensor, rhs: Element) -> Tensor {
    return lhs • rhs.broadcastTo(rank: lhs.rank)
  }

  @_inlineable
  static func < (lhs: Tensor, rhs: Tensor) -> Tensor<Bool> {
    return Tensor<Bool>(tfop_elt_less(lhs.core, rhs.core))
  }
  @_inlineable
  static func < (lhs: Tensor, rhs: Element) -> Tensor<Bool> {
    return lhs < rhs.broadcastTo(rank: lhs.rank)
  }

  // FIXME: Other comparisons as well.  Should Tensor conform to Numeric or
  // some other standard protocols?
}

/// Arithmetic methods.
public extension Tensor {
  @_inlineable
  func transpose() -> Tensor {
    return Tensor(tfop_transpose(self.core))
  }

  @_inlineable
  func mean() -> Element {
    return tfop_mean(self.core)
  }
  @_inlineable
  func min() -> Element {
    return tfop_min(self.core)
  }
  @_inlineable
  func max() -> Element {
    return tfop_max(self.core)
  }
  @_inlineable
  func max(axis: Int...) -> Tensor {
    return Tensor(tfop_max_axis(self.core, axis))
  }
  @_inlineable
  func argmax() -> Int64 {
    return tfop_argmax(self.core)
  }
  // Sum entire tensor to produce a scalar value.
  @_inlineable
  func sum() -> Element {
    return tfop_sum(self.core)
  }
  // Sum tensor along one or more axis, producing a Tensor.
  @_inlineable
  func sum(axis: Int...) -> Tensor {
    return Tensor(tfop_sum_axis(self.core, axis))
  }

  @_inlineable
  func square() -> Tensor {
    // TODO: Is the TF op for this more efficient?
    return self•self.transpose()
  }

  func reduceMean() -> Element {
    // FIXME: Implement!
    preconditionFailure()
  }

  @_inlineable
  func log() -> Tensor {
    return Tensor(tfop_log(self.core))
  }

  @_inlineable
  func tanh() -> Tensor {
    return Tensor(tfop_tanh(self.core))
  }

  @_inlineable
  func exp() -> Tensor {
    return Tensor(tfop_exp(self.core))
  }

  @_inlineable
  func concat(rhs: Tensor) -> Tensor {
    return Tensor(tfop_concat(self.core, rhs.core))
  }
}


/// TODO: When TensorCore can produce a summary, we should wire this in.
#if false
/// Make "print(someTensor)" print a pretty form of the tensor.
extension Tensor : CustomStringConvertible {
  public var description: String {
    return core.description
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension Tensor : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return core.customPlaygroundQuickLook
  }
}
#endif
