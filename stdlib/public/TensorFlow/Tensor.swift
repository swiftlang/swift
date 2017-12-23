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

import Swift

public struct Tensor<Element : TensorElementProtocol> {
  // State owned by this tensor.
  fileprivate let core : TensorCore<Element>

  internal init(_ c : TensorCore<Element>) {
    core = c
  }
}

// Initializers
public extension Tensor {
  /// Perform an element conversion from Tensor<U> to Tensor<T>.
  public init<FromType>(_ t : Tensor<FromType>) {
    fatalError("tensor conversion unimplemented")
  }

  // zeroD initializer, takes exactly one value.
  init(zeroD value: Element) {
    self.init(TensorCore<Element>())
  }
}
