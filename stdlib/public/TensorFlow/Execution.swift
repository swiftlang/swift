//===-- Execution.swift ---------------------------------------*- swift -*-===//
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
// This file defines public APIs regarding TensorFlow runtime support.
//
//===----------------------------------------------------------------------===//

/// A TensorFlow device kind.
public enum DeviceKind {
  /// The CPU device kind.
  case cpu
  /// The GPU device kind.
  case gpu
}

/// Executes a closure, making TensorFlow operations run on a specific kind of
/// device.
///
/// - Parameters:
///   - kind: A kind of device to run TensorFlow operations on.
///   - index: The device to run the ops on.
///   - body: A closure whose TensorFlow operations are to be executed on the
///     specified kind of device.
// Use `@inline(never)` to ensure correctness in scoped device placement. See
// https://bugs.swift.org/browse/SR-9535 for more context.
@inline(never)
public func withDevice<R>(_ kind: DeviceKind, _ index: UInt = 0,
                          perform body: () throws -> R) rethrows -> R {
  _ThreadLocalState.value.pushDevice((kind, index))
  let result = try body()
  _ThreadLocalState.value.popDevice()
  return result
}

/// Executes a closure, allowing TensorFlow to place TensorFlow operations on
/// any device. This should restore the default placement behavior.
///
/// - Parameters:
///   - body: A closure whose TensorFlow operations are to be executed on the
///     specified kind of device.
@inline(never)
public func withDefaultDevice<R>(perform body: () throws -> R) rethrows -> R {
  _ThreadLocalState.value.pushDevice(nil)
  let result = try body()
  _ThreadLocalState.value.popDevice()
  return result
}
