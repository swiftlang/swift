//===-- TensorProtocol.swift ----------------------------------*- swift -*-===//
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
// This file defines the TensorProtocol type.
//
//===----------------------------------------------------------------------===//

public protocol TensorProtocol {
  /// Scalar type.
  associatedtype Scalar : TensorFlowScalar

  /// The underlying `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  var handle: TensorHandle<Scalar> { get }

  /// Initialize from a `TensorHandle`.
  /// - Note: Do NOT remove this. This is a compiler requirement.
  init(handle: TensorHandle<Scalar>)
}

// FIXME: Consider moving to CompilerProtocols.swift when the interaction
// between compiler and runtime becomes general enough.
//
/// The protocol on tensor sends and receives.
///
/// - Note: The compiler knows about this protocol and generates code to use
/// it. So changing the protocol design requires changing the compiler
/// accordingly too.
protocol TensorSendableReceivable {
  associatedtype Scalar

  /// Receive a tensor based on a tensor computation handle (equivalent to a TF
  /// session handle), and a tensor ID.
  static func receiveFromAccelerator(_ computation: _TensorComputation,
                                     _ tensorID: Int) -> Self

  /// Send a tensor of `this` instance based on a tensor computation handle
  /// (equivalent to a TF session handle), and a tensor ID.
  func sendToAccelerator(_ computation: _TensorComputation,
                         _ tensorID: Int)

  /// Create a scalar tensor. It can be used by the host program to send a
  /// scalar value to accelerator.
  ///
  /// - Note: This is different from protocol method
  /// `TensorFlowScalar._makeScalarTensor()`, a marker function to assist
  /// compiler in generating Accelerator code, and has no runtime effects.
  static func scalar(_ scalar: Scalar) -> Self
}
