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
  /// A tensor just contains a TensorHandle under the covers.  This is public to
  /// allow user defined ops, but shouldn't normally be used otherwise.
  public let handle : TensorHandle<Element>

  @_inlineable
  public init(_ c : TensorHandle<Element>) {
    handle = c
  }
}

//===----------------------------------------------------------------------===//
// Shape and Rank accessors
//===----------------------------------------------------------------------===//
//
// TODO: If these are only needed on the host, we could implement it in terms
// of the TF_TensorHandle API.  In that case, we should sink them to be defined
// on TensorHandle though, because we don't want people to think that they can
// call this and have it run on the accelerator.

public extension Tensor {
  // make @_inlinable when implemented.
  var rank : Int {
    fatalError("FIXME: implement rank")
  }
  // make @_inlinable when implemented.
  var shape : [Int] {
    fatalError("FIXME: implement shape")
  }
}


//===----------------------------------------------------------------------===//
// Send and receive magic.
//===----------------------------------------------------------------------===//
//
// By default, when a tensor value is implicitly passed between host and tensor
// code, the partitioning pass will generate a warning.  Users can indicate that
// they are doing something intentional by using these methods, which silences
// the warning.

// TODO: These would be nicer if defined as builtins rather than being "well
// known functions".
@_versioned @inline(never)
@_silgen_name("__tfop_send") // Magic name, not a TF op!
func tfop_send<T>(_ c : TensorHandle<T>) -> TensorHandle<T> {
  return c
}
@_versioned @inline(never)
@_silgen_name("__tfop_receive") // Magic name, not a TF op!
func tfop_receive<T>(_ c : TensorHandle<T>) -> TensorHandle<T> {
  return c
}

public extension Tensor {
  /// Indicate that this tensor is being moved to the accelerator.
  @_inlineable
  public func toDevice() -> Tensor<Element> { return Tensor(tfop_send(handle)) }

  /// Indicate that this tensor is being moved to the host.
  @_inlineable
  public func toHost() -> Tensor<Element> { return Tensor(tfop_receive(handle)) }
}


//===----------------------------------------------------------------------===//
// Ops and Convenience Methods
//===----------------------------------------------------------------------===//
//
// The majority of the Tensor API is implemented in terms of 'ops' that are
// partitioned out to the TensorFlow graph when the compiler runs.  These
// ops are intentially designed to reflect TensorFlow ops, but provide nicer
// Swift syntax for accessing them.  In addition to the core ops themselves,
// we also define some helper function wrappers, e.g. to make things symmetric
// and generally feel nice to use.
//
// The ops themselves are defined by the primitive #tfop(...) syntax, here are
// some examples:
//     result = #tfop("Add", "tt:t", lhs, rhs)
//     result = #tfop("Const", "dc:t", Float.self, 4.0)
//
// The first two parameters to this syntax are the TensorFlow op name as a
// string, and then a constraint string - which specifies information about the
// operands and result type of the op.  The inputs are specified as additional
// arguments that follow.
//
// The constraint string is specified as two colon separated lists:
// "<OPERANDS>:<RESULTS>".  Here are the codes that are recognized for operands
// so far:
//
//    t: the next operand is a TensorHandle, and is an "input" to the TF node.
//    d: the next operand is a metatype value, and is added as a 'dtype'
//       attribute to the TF node. [TODO: Use param label to genericize name].
//    c: the next operand is a standard library integer or FP type.  We should
//       pass the value(s) as the 'value' attribute.  [TODO: Use param label to
//       genericize to names other than 'value'].
//
// The codes for the results are currently:
//
//    t: the result is a TensorHandle<T>, where the T is the same type as one
//       of the tensor input operands, or the type of the last dtype specified.
//    t<type>: the result is a TensorHandle<T>, where T is written out manually
//       using the same type names that TensorFlow ops use.
//


// Initializers
public extension Tensor {
  /// Perform an element conversion from Tensor<U> to Tensor<T>.
  @inline(never) // make @_inlinable when implemented.
  init<FromType>(_ t : Tensor<FromType>) {
    fatalError("FIXME: Implement element conversion")
  }

  // zeroD initializer, takes exactly one value.
  @_inlineable
  init(zeroD value: Element) {
    handle = #tfop("Const", "dc:t", Element.self, value)
  }

  // oneD initializer, takes an array of values.
  @inline(never) // make @_inlinable when implemented.
  init(oneD values: [Element]) {
    fatalError("FIXME: implement init(oneD:)")
  }

  // oneD initializer, takes a vararg list of values.
  @_inlineable
  init(oneD values: Element...) {
    self.init(oneD: values)
  }

  @inline(never) // make @_inlinable when implemented.
  init(twoD values: [[Element]]) {
    fatalError("FIXME: implement init(twoD:)")
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
  @inline(never) // make @_inlinable when implemented.
  init(zeros dimensions: [Int]) {
    fatalError("FIXME: implement zeros")
  }
  /// Ones initializer, takes a list of dimensions.
  @_inlineable
  init(ones dimensions: Int...) {
    self.init(ones: dimensions)
  }
  @inline(never) // make @_inlinable when implemented.
  init(ones dimensions: [Int]) {
    fatalError("FIXME: implement ones")
  }

  // def tf.random_normal(shape, mean=0.0, stddev=1.0, dtype=dtypes.float32,
  //                      seed=None, name=None):
  @inline(never) // make @_inlinable when implemented.
  public init(randomNormal dimensions: Int...,
    mean: Double = 0, stddev: Double = 1) {
    fatalError("FIXME: implement randomNormal")
  }

  // def tf.eye(shape)
  @inline(never) // make @_inlinable when implemented.
  public init(eye dimensions: [Int]) {
    fatalError("FIXME: implement eye init")
  }
}

// Subscripting a tensor produces a smaller tensor.
public extension Tensor {
  // make @_inlinable when implemented.
  subscript(tensor indices : Int...) -> Tensor {
    fatalError("FIXME: implement subscript to tensor")
  }

  // Subscript to get a scalar.
  // make @_inlinable when implemented.
  subscript(scalar indices : Int...) -> Element {
    fatalError("FIXME: implement subscript to scalar")
  }

  // Slicing out a range of elements.
  // TODO: begin/end are vectors in general.
  // tfop_slice(tensor, begin, end) -> tensor
  // make @_inlinable when implemented.
  subscript(slice : Range<Int>) -> Tensor {
    fatalError(
      "FIXME: implement slice \(slice.lowerBound) ... \(slice.upperBound)")
  }
}

public extension TensorElementProtocol {
  /// Broadcast the specified scalar value to be a tensor with the same rank as
  /// the specified other tensor, but with dimension=1 for each rank.
  @inline(never) // make @_inlinable when implemented.
  func broadcastTo(rank: Int) -> Tensor<Self> {
    fatalError("FIXME: implement scalar broadcast")
  }
}
public extension Tensor {
  /// Broadcast the specified Tensor to a rank >= its current size, filling in
  /// the new dimensions with rank = 1.
  @inline(never) // make @_inlinable when implemented.
  func broadcastTo(rank: Int) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }

  /// Broadcast self tensor to the same shape as the specified one.
  @inline(never) // make @_inlinable when implemented.
  func broadcastTo(tensor: Tensor) -> Tensor {
    fatalError("FIXME: implement broadcast")
  }
}


/// Arithmetic Operators.
public extension Tensor {
  @_inlineable
  static func +(lhs: Tensor, rhs: Tensor) -> Tensor {
    return Tensor(#tfop("Add", "tt:t", lhs.handle, rhs.handle))
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
    return Tensor(#tfop("Sub", "tt:t", lhs.handle, rhs.handle))
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
    return Tensor(#tfop("Mul", "tt:t", lhs.handle, rhs.handle))
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
    return Tensor(#tfop("Div", "tt:t", lhs.handle, rhs.handle))
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


  @inline(never) // make @_inlinable when implemented.
  static func •(lhs: Tensor, rhs: Tensor) -> Tensor {
    fatalError("FIXME: implement matmul")
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
    return Tensor<Bool>(#tfop("Less", "tt:t<bool>", lhs.handle, rhs.handle))
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
  @inline(never) // make @_inlinable when implemented.
  func transpose() -> Tensor {
    fatalError("FIXME: implement transpose")
  }

  @inline(never) // make @_inlinable when implemented.
  func mean() -> Element {
    fatalError("FIXME: implement mean")
  }
  @inline(never) // make @_inlinable when implemented.
  func min() -> Element {
    fatalError("FIXME: implement min")
  }
  @inline(never) // make @_inlinable when implemented.
  func max() -> Element {
    fatalError("FIXME: implement max")
  }
  @inline(never) // make @_inlinable when implemented.
  func max(axis: Int...) -> Tensor {
    fatalError("FIXME: implement max axis")
  }

  /// TODO: Should this return a Tensor0D to keep it in the tensor domain?
  @inline(never) // make @_inlinable when implemented.
  func argmax() -> Int64 {
    fatalError("FIXME: implement argmax")
  }
  // Sum entire tensor to produce a scalar value.
  @inline(never) // make @_inlinable when implemented.
  func sum() -> Element {
    fatalError("FIXME: implement sum")
  }
  // Sum tensor along one or more axis, producing a Tensor.
  @inline(never) // make @_inlinable when implemented.
  func sum(axis: Int...) -> Tensor {
    fatalError("FIXME: implement sum axis")
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
    return Tensor(#tfop("Log", "t:t", self.handle))
  }

  @_inlineable
  func tanh() -> Tensor {
    return Tensor(#tfop("Tanh", "t:t", self.handle))
  }

  @_inlineable
  func exp() -> Tensor {
    return Tensor(#tfop("Exp", "t:t", self.handle))
  }

  @inline(never) // make @_inlinable when implemented.
  func concat(rhs: Tensor) -> Tensor {
    fatalError("FIXME: implement concat")
  }
}


/// TODO: When TensorHandle can produce a summary, we should wire this in.
#if false
/// Make "print(someTensor)" print a pretty form of the tensor.
extension Tensor : CustomStringConvertible {
  public var description: String {
    return handle.description
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension Tensor : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return handle.customPlaygroundQuickLook
  }
}
#endif
