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
@usableFromInline
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

@usableFromInline
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
@usableFromInline typealias CTensor = OpaquePointer

/// The `TF_TensorHandle *` type.
///
/// - Note: This is public so that compiler generated code can read/write tensor
/// handles when calling runtime APIs.
public typealias CTensorHandle = OpaquePointer

/// The `TFE_Context *` type.
public typealias CTFEContext = OpaquePointer

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

@usableFromInline
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
// File writing
//===----------------------------------------------------------------------===//

/// Given the address of a `TF_Buffer` and a file path, write the buffer's
/// contents to the file.
func writeContents(of buffer: UnsafePointer<TF_Buffer>,
                   toFile path: String) {
  let fp = fopen(path, "w+")
  fwrite(buffer.pointee.data, /*size*/ 1, /*count*/ buffer.pointee.length, fp)
  fclose(fp)
}

//===----------------------------------------------------------------------===//
// Unit test utilities
//===----------------------------------------------------------------------===//
// TODO: Move this section to a unit-test only Swift module, once the google
// internal lit based test infra can handle importing additional Swift modules.

/// This is a generic host-only op that hides the details of its impl in the SIL
/// code. This makes reading/writing SIL based compiler unit tests simple.
@inline(never)
public func _hostOp<T>(_ x: T) {
  print(x)
}

@inline(never)
public func _hostOp<Scalar>(_ x: Tensor<Scalar>) {
  print(x)
}

@inline(never)
public func _hostOp<Scalar : TensorFlowScalar>(_ x: TensorHandle<Scalar>) {
  print(Tensor(handle: x))
}

/// Some TPU ops (e.g. infeed/outfeed) require tensor shape info, which the APIs
/// below can provide.
///
/// TODO: Remove these helper APIs, when we have a better shape
/// inference/propagation design.
@inlinable @inline(__always)
public func _scalarTensorWithShape<Scalar>(
  _ x: Tensor<Scalar>
) -> Tensor<Scalar> {
  let ret: TensorHandle<Scalar> =
    #tfop("Identity", x, T$dtype: Scalar.tensorFlowDataType,
          __shapes: [TensorShape()])
  return Tensor<Scalar>(handle: ret)
}

@inlinable @inline(__always)
public func _addScalarTensorsWithShape<Scalar>(
  _ x: Tensor<Scalar>,
  _ y: Tensor<Scalar>
) -> Tensor<Scalar> {
  let ret: TensorHandle<Scalar> =
    #tfop("Add", x, y, T$dtype: Scalar.tensorFlowDataType,
          __shapes: [TensorShape()])
  return Tensor<Scalar>(handle: ret)
}
