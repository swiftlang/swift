//===-- DevicePlacement.swift ---------------------------------*- swift -*-===//
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
// This file defines APIs for device placement.
//
//===----------------------------------------------------------------------===//

/// A TensorFlow device kind.
public enum DeviceKind {
  /// Default device.
  case `default`
  /// Graphics processing units.
  case gpu
  /// Tensor processing units.
  case tpu
}

/// Executes code in the current scope on GPU.
///
/// - Parameter serverAddress: The address to the TPU server where a remote
///   session is alive. If `nil`, use a local session.
///
// FIXME: We need transparent here because deabstraction isn't inlining this
// function.  We need to inline if a callee contains tensor ops, not only if
// it takes and returns a TensorFlow value.
@_transparent
public func enableTPU(serverAddress: String? = nil) {
  _RuntimeConfig.executionMode = .tpu
  if let serverAddress = serverAddress {
    _RuntimeConfig.session = .remote(grpcAddress: serverAddress)
  }
  #tfop("tfc.configureTPU", enableInfeed: true) as Void
}

/// Executes code in the current scope on GPU.
///
// FIXME: Extend the interface to support multiple GPU devices, and unify it
// with enableTPU() above.
@_transparent
public func enableGPU() {
  #tfop("tfc.configureGPU") as Void
}

/// Executes a closure on GPU.
///
/// - Parameter body: The closure to execute.
/// - Throws: The error that the body throws, if any.
/// - Returns: The value returned by the closure, if any.
///
@_semantics("tf.withDevice(_:execute:)") @_optimize(none) @inline(never)
public func withDevice<Result>(
  _ kind: DeviceKind, execute body: () throws -> Result
) rethrows -> Result {
  return body()
}
