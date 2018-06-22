//===-- Utilities.swift ---------------------------------------*- swift -*-===//
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
// This file defines utility functions and common type aliases.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif
import CTensorFlow

//===----------------------------------------------------------------------===//
// Standard library extensions
//===----------------------------------------------------------------------===//

public extension Sequence {
  /// Returns true if all elements satisfy the predicate.
  func forAll(_ predicate: (Element) throws -> Bool) rethrows -> Bool {
    return try first(where: { try !predicate($0) }) == nil
  }
}

//===----------------------------------------------------------------------===//
// Runtime checkers
//===----------------------------------------------------------------------===//

/// These checks run in both debug and release modes (while assert() only runs
/// in debug mode), to help shake out more bugs and facilitate debugging in the
/// early project phases. It can be replaced with plain assert() later, when we
/// have a more mature code base.
@_versioned
func internalConsistencyCheck(
  _ predicate: Bool,
  _ errMessage: String = "TF runtime assertion failure",
  file: StaticString = #file,
  line: UInt = #line
) {
  guard predicate else {
    fatalError(errMessage, file: file, line: line)
  }
}

@_versioned
func checkOk(_ s: CTFStatus?, file: StaticString = #file, line: UInt = #line) {
  internalConsistencyCheck(TF_GetCode(s) == TF_OK,
                           String(cString: TF_Message(s)),
                           file: file, line: line)
}

//===----------------------------------------------------------------------===//
// Type aliases
//===----------------------------------------------------------------------===//

// Before assigning a C pointer to one of the pointer type aliases below, caller
// should check that the pointer is not NULL.

/// The `TF_Session *` type.
typealias CTFSession = OpaquePointer

/// The `TF_Status *` type.
typealias CTFStatus = OpaquePointer

/// The `TF_Graph*` type.
typealias CTFGraph = OpaquePointer

/// The `TF_Function*` type.
typealias CTFFunction = OpaquePointer

/// The `TF_Tensor *` type.
typealias CTensor = OpaquePointer

/// The `TF_TensorHandle *` type.
///
/// - Note: This is public so that compiler generated code can read/write tensor
/// handles when calling runtime APIs.
public typealias CTensorHandle = OpaquePointer

/// The `TFE_Context *` type.
typealias CTFEContext = OpaquePointer

/// The `TFE_Op *` type.
typealias CTFEOp = OpaquePointer

//===----------------------------------------------------------------------===//
// Logging
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
let stderr = __stderrp
let stdout = __stdoutp
#endif

/// Log to standard error.
func logToStderr(_ message: StaticString) {
  message.utf8Start
    .withMemoryRebound(to: Int8.self, capacity: message.utf8CodeUnitCount) {
  _ = fputs($0, stderr)
  }
}

/// Log to standard error.
func logToStderr(_ message: String) {
  _ = fputs(message, stderr)
}

@_versioned
func debugLog(_ message: @autoclosure () -> String,
              file: StaticString = #file,
              line: UInt = #line) {
  if _RuntimeConfig.printsDebugLog {
    print("[\(file):\(line)] \(message())")
    // This helps dump more log before a crash.
    fflush(stdout)
  }
}

//===----------------------------------------------------------------------===//
// Pseudorandom number generation
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
/// The BSD random number function.
///
/// - Returns: A random 32-bit integer.
/// - Note:
///   - On Darwin platforms, this function is hidden to encourage the use of
///     arc4random for cryptographically secure random numbers. However,
///     reproduciblity through seeding is required by our users.
///   - Although the corresponding C function returns `long int`, aka `Int`, its
///     maximum is `Int32.max`. Therefore it is safe and recommended to cast it
///     to `Int32` without truncation.
///
@_silgen_name("random")
private func random() -> Int
#endif

/// A pseudorandom state.
@_fixed_layout
open class RandomState {
  /// The global pseudorandom state.
  public static let global = RandomState(seed: UInt32(time(nil)),
                                         bufferSize: 256)

  /// The start address of a memory buffer that stores the internal state
  /// information.
  private var bufferAddress: UnsafeMutablePointer<Int8>
  /// The size of the state buffer.
  private var bufferSize: Int

  deinit {
    bufferAddress.deinitialize(count: bufferSize)
    bufferAddress.deallocate()
  }

  /// Creates a pseudorandom state with the given seed in a buffer of the given
  /// size.
  ///
  /// - Parameters:
  ///   - seed: A number used to initialize the pseudorandom state.
  ///   - bufferSize: The size of the buffer used to hold the internal state
  ///     information. It is initialized based on `seed`. `size` must be
  ///     between 8 and 256, and should be a power of two.
  ///
  @_versioned
  init(seed: UInt32, bufferSize: Int = 64) {
    self.bufferAddress = .allocate(capacity: bufferSize)
    self.bufferSize = bufferSize
    self.seed(with: seed)
  }

  /// Creates a pseudorandom state by copying from another pseudorandom state.
  ///
  /// - Parameter other: The pseudorandom state to copy from.
  ///
  public init(_ other: RandomState) {
    bufferSize = other.bufferSize
    bufferAddress = UnsafeMutablePointer<Int8>.allocate(capacity: bufferSize)
    bufferAddress.initialize(from: other.bufferAddress, count: bufferSize)
  }

  /// Creates a pseudorandom state with the given seed.
  ///
  /// - Parameter seed: A number used to initialize the pseudorandom state.
  ///
  public convenience init(seed: UInt32) {
    self.init(seed: seed, bufferSize: 64)
  }

  /// Seed the pseudorandom state.
  ///
  /// - Parameter seed: The seed to be used to initialize the internal
  ///   psuedorandom state.
  /// - Note: Users should not re-seed the `global` state unless there is a
  ///   compelling reason to do so.
  ///
  open func seed(with seed: UInt32) {
    initstate(seed, bufferAddress, bufferSize)
  }

  /// Returns the next pseudorandom number and updates the internal state.
  ///
  /// - Returns: The next pseudorandom number.
  ///
  open func generate() -> Int32 {
    let previousState = setstate(bufferAddress)
    defer { setstate(previousState!) }
    return Int32(random())
  }

  /// Returns an array containing the next specified count of pseudorandom
  /// numbers and updates the internal state.
  ///
  /// - Parameter count: The number of pseudorandom numbers to generate.
  /// - Returns: An array containing the next pseudorandom numbers.
  ///
  open func generate(_ count: Int) -> [Int32] {
    let previousState = setstate(bufferAddress)
    defer { setstate(previousState!) }
    return (0..<count).map { _ in Int32(random()) }
  }
}

/// A sequence of pseudorandom numbers.
@_fixed_layout
public struct RandomNumberSequence : Sequence {
  /// The seed value for random sequence generation.
  public let seed: UInt32?

  /// Creates a sequence of pseudorandom numbers. If a seed is given, a random
  /// state will be created and initialized based on the seed value. Otherwise
  /// the global pseudorandom state, i.e. `RandomState.global`, will be used and
  /// updated. Use a seed if the use case requires the random sequence to be
  /// reproducible.
  ///
  /// - Parameter seed: The seed value for random sequence generation.
  ///
  public init(seed: UInt32? = nil) {
    self.seed = seed
  }

  public func makeIterator() -> AnyIterator<Int32> {
    return AnyIterator {
      let state = self.seed.flatMap(RandomState.init) ?? .global
      return state.generate()
    }
  }
}

//===----------------------------------------------------------------------===//
// Unit test utilities
//===----------------------------------------------------------------------===//
// TODO: Move this section to a unit-test only Swift module, once the google
// internal lit based test infra can handle importing additional Swift modules.

/// This is a generic host-only op that hides the details of its impl in the SIL
/// code. This makes reading/writing SIL based compiler unit tests simple.
@inline(never)
public func _hostOp<Scalar>(_ x: Tensor<Scalar>) {
  print(x)
}

/// Some TPU ops (e.g. infeed/outfeed) require tensor shape info, which the APIs
/// below can provide.
///
/// TODO: Remove these helper APIs, when we have a better shape
/// inference/propagation design.
@_inlineable @inline(__always)
public func _scalarTensorWithShape<T>(_ x: Tensor<T>) -> Tensor<T> {
  let ret : Tensor<T> = #tfop("Identity", x, __shapes: [TensorShape()])
  return ret
}

@_inlineable @inline(__always)
public func _addScalarTensorsWithShape<T>(_ x: Tensor<T>, _ y: Tensor<T>
) -> Tensor<T> {
  let ret : Tensor<T> = #tfop("Add", x, y, __shapes: [TensorShape()])
  return ret
}
