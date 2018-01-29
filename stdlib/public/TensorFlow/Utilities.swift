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

#if os(Linux) || os(FreeBSD)
  import Glibc
#else
  import Darwin
#endif
import CTensorFlow

//===----------------------------------------------------------------------===//
// Standard library extensions
//===----------------------------------------------------------------------===//

public extension Sequence {
  /// Returns true if all elements satisfy the predicate
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

/// TF_Status* type.
typealias CTFStatus = OpaquePointer

/// TF_Function* type
typealias CTFFunction = OpaquePointer

/// TF_Tensor* type.
typealias CTensor = OpaquePointer

/// TFE_Context* type.
typealias CTFEContext = OpaquePointer

/// TFE_Op* type.
typealias CTFEOp = OpaquePointer

//===----------------------------------------------------------------------===//
// Logging
//===----------------------------------------------------------------------===//

#if os(macOS)
let stderr = __stderrp
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
              line: UInt = #line
) {
  if _RuntimeConfig.printsDebugLog {
    print("[\(file):\(line)] \(message())")
    // This helps dump more log before a crash.
    fflush(stdout)
  }
}

