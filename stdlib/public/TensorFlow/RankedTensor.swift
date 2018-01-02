//===-- RankedTensor.swift ------------------------------------*- swift -*-===//
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

/// RankedTensor - This is the common protocol shared between the Tensor[12345]D
/// types defined below, defining their common basic API.  This allows reuse
/// of the implementation logic for these types (which just wrap Tensor anyway)
/// and allow writing rank-generic code over the TensorXD types.
public protocol RankedTensor {

  associatedtype Element : TensorElementProtocol

  /// Convert from a rank-erased Tensor to the specified RankedTensor.  This
  /// fails when the Tensor has the wrong rank.
  init?(_ t: Tensor<Element>)

  /// Convert from a rank-erased Tensor to the specified RankedTensor when there
  /// is some static information that tells us that it is of the correct rank
  /// already.
  init(knownRank: Tensor<Element>)

  /// Tensor of same rank, but Bool element type.
  associatedtype BoolTensor : RankedTensor where BoolTensor.Element == Bool

  /// Property that returns the rank of the Tensor - the number of dimensions
  /// it has.
  var rank : Int { get }

  /// Returns the type erased Tensor held by the RankedTensor.
  var underlyingTensor : Tensor<Element> { get }
}

public extension RankedTensor {
  /// Indicate that this tensor is being moved to the accelerator.
  @_inlineable
  func toDevice() -> Self { return Self(knownRank: underlyingTensor.toDevice())}

  /// Indicate that this tensor is being moved to the host.
  @_inlineable
  func toHost() -> Self { return Self(knownRank: underlyingTensor.toHost()) }
}

// Slicing
public extension RankedTensor {
  @_inlineable
  subscript(slice : Range<Int>) -> Self {
    return Self(knownRank: underlyingTensor[slice])
  }
}


// Each of these extensions on RankedTensor adds APIs to each of the specific
// ranks, implemented in terms of the underlying untyped Tensor APIs.

/// RankedTensor - Arithmetic Operators.
public extension RankedTensor {
  @_inlineable
  static func +(lhs: Self, rhs : Self) -> Self {
    return Self(knownRank: lhs.underlyingTensor + rhs.underlyingTensor)
  }
  @_inlineable
  static func +=(lhs: inout Self, rhs : Self) {
    lhs = lhs + rhs
  }
  @_inlineable
  static func +(lhs: Self, rhs : Element) -> Self {
    return Self(knownRank: lhs.underlyingTensor + rhs)
  }
  @_inlineable
  static func +(lhs: Element, rhs : Self) -> Self {
    return Self(knownRank: lhs + rhs.underlyingTensor)
  }
  @_inlineable
  static func -(lhs: Self, rhs : Self) -> Self {
    return Self(knownRank: lhs.underlyingTensor - rhs.underlyingTensor)
  }
  @_inlineable
  static func -(lhs: Self, rhs : Element) -> Self {
    return Self(knownRank: lhs.underlyingTensor - rhs)
  }
  @_inlineable
  static func -(lhs: Element, rhs : Self) -> Self {
    return Self(knownRank: lhs - rhs.underlyingTensor)
  }
  @_inlineable
  static func -=(lhs: inout Self, rhs : Self) {
    lhs = lhs - rhs
  }
  @_inlineable
  static func /(lhs: Self, rhs : Self) -> Self {
    return Self(knownRank: lhs.underlyingTensor / rhs.underlyingTensor)
  }
  @_inlineable
  static func /(lhs: Self, rhs : Element) -> Self {
    return Self(knownRank: lhs.underlyingTensor / rhs)
  }
  @_inlineable
  static func /(lhs: Element, rhs : Self) -> Self {
    return Self(knownRank: lhs / rhs.underlyingTensor)
  }
  @_inlineable
  static func *(lhs: Self, rhs : Self) -> Self {
    return Self(knownRank: lhs.underlyingTensor * rhs.underlyingTensor)
  }
  @_inlineable
  static func *(lhs: Element, rhs : Self) -> Self {
    return Self(knownRank: lhs * rhs.underlyingTensor)
  }
  @_inlineable
  static func *(lhs: Self, rhs : Element) -> Self {
    return Self(knownRank: lhs.underlyingTensor * rhs)
  }
  @_inlineable
  static func •(lhs: Self, rhs : Self) -> Self {
    return Self(knownRank: lhs.underlyingTensor • rhs.underlyingTensor)
  }
  @_inlineable
  static func •(lhs: Element, rhs : Self) -> Self {
    return Self(knownRank: lhs • rhs.underlyingTensor)
  }
  @_inlineable
  static func •(lhs: Self, rhs : Element) -> Self {
    return Self(knownRank: lhs.underlyingTensor • rhs)
  }
  @_inlineable
  static func < (lhs: Self, rhs : Element) -> BoolTensor {
    return BoolTensor(knownRank: lhs.underlyingTensor < rhs)
  }

  // ...
}

/// RankedTensor - Arithmetic methods.
public extension RankedTensor {
  /// Property that returns the size of each dimension in the Tensor, the count
  /// of elements in the array returned is always equal to 'rank'.
  @_inlineable
  var shape : [Int] {
    return underlyingTensor.shape
  }

  @_inlineable
  func transpose() -> Self {
    return Self(knownRank: underlyingTensor.transpose())
  }

  @_inlineable
  func mean() -> Element {
    return underlyingTensor.mean()
  }
  @_inlineable
  func min() -> Element {
    return underlyingTensor.min()
  }
  @_inlineable
  func max() -> Element {
    return underlyingTensor.max()
  }
  @_inlineable
  func argmax() -> Int64 {
    return underlyingTensor.argmax()
  }
  @_inlineable
  func sum() -> Element {
    return underlyingTensor.sum()
  }

  @_inlineable
  func square() -> Self { return self.transpose()•self }

  @_inlineable
  func reduceMean() -> Element {
    return underlyingTensor.reduceMean()
  }


  @_inlineable
  func dot(rhs : Self) -> Self {
    return Self(knownRank: self.underlyingTensor • rhs.underlyingTensor)
  }
  @_inlineable
  func dot(rhs : Element) -> Self {
    return Self(knownRank: self.underlyingTensor • rhs)
  }

  @_inlineable
  func concat(rhs: Self) -> Self {
    return Self(knownRank: self.underlyingTensor.concat(rhs: rhs.underlyingTensor))
  }

  @_inlineable
  func tanh() -> Self {
    return Self(knownRank: self.underlyingTensor.tanh())
  }

  @_inlineable
  func exp() -> Self {
    return Self(knownRank: self.underlyingTensor.exp())
  }
  @_inlineable
  func log() -> Self {
    return Self(knownRank: self.underlyingTensor.log())
  }
  // ...
}

#if false // TODO: Enable this when Tensor's can summarize themselves.

/// Make "print(someTensor)" print a pretty form of the tensor.
extension RankedTensor {
  public var description: String {
    return underlyingTensor.description
  }
}

// Make Tensors show up nicely in the Xcode Playground results sidebar.
extension RankedTensor {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

#endif
