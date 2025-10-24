//===--- Backtrace.swift --------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines functions and types allowing to handle Swift's mangling scheme.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif
internal import BacktracingImpl.Runtime

// - MARK: Demangling

/// Given a mangled Swift symbol, demangle it into a human readable format.
///
/// Valid Swift symbols begin with the following prefixes:
///   ┌─────────────────────╥────────┐
///   │ Swift Version       ║        │
///   ╞═════════════════════╬════════╡
///   │ Swift 3 and below   ║   _T   │
///   ├─────────────────────╫────────┤
///   │ Swift 4             ║  _T0   │
///   ├─────────────────────╫────────┤
///   │ Swift 4.x           ║   $S   │
///   ├─────────────────────╫────────┤
///   │ Swift 5+            ║   $s   │
///   └─────────────────────╨────────┘
///
/// - Parameters:
///   - mangledName: A mangled Swift symbol.
/// - Returns: A human readable demangled Swift symbol.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
// @_alwaysEmitIntoClient // FIXME: how to keep this usable by backtrace
public func demangle(_ mangledName: String) -> String? {
  var length: size_t = 0

  let demangled = _swift_runtime_demangle(
    mangledName, mangledName.utf8.count,
    nil, &length,
    /*flags=*/0
  )

  guard let demangled else {
    return nil
  }
  defer { free(demangled) }

  // length is the size of the buffer that was allocated, *not* the
  // length of the string.
  let stringLen = strlen(demangled)
  guard stringLen > 0 else {
    return nil
  }

  return demangled.withMemoryRebound(to: UInt8.self, capacity: stringLen) {
    let demangledBytes = UnsafeBufferPointer<UInt8>(start: $0, count: stringLen)
    return String(decoding: demangledBytes, as: UTF8.self)
  }
}

//@available(SwiftStdlib 6.3, *)
//@lifetime(borrow mangledName)
//public func demangle(_ mangledName: UTF8Span) -> String? {
//  var length: size_t = 0
//
//  let demangledPtr =
//    mangledName._withUnsafeBufferPointer { mangledNamePtr in
//      _swift_runtime_demangle(
//        mangledNamePtr.baseAddress, mangledName.count,
//        nil, &length,
//        /*flags=*/0)
//    }
//
//  let demangledBufferPtr = UnsafeMutableBufferPointer(start: demangledPtr, count: length)
//  let demangledOutput = OutputSpan(buffer: demangledPtr, initializedCount: length)
//  let demangledSpan = UTF8Span(validating: demangledOutput.span)
//
//  guard let demangledPtr else {
//    return nil
//  }
//  defer { free(demangledPtr) }
//
//  // length is the size of the buffer that was allocated, *not* the
//  // length of the string.
//  let stringLen = strlen(demangledPtr)
//  guard stringLen > 0 else {
//    return nil
//  }
//
//  return String(copying: demangledSpan)
//}

/// Given a mangled Swift symbol, demangle it into a human readable format.
///
/// Valid Swift symbols begin with the following prefixes:
///   ┌─────────────────────╥────────┐
///   │ Swift Version       ║        │
///   ╞═════════════════════╬════════╡
///   │ Swift 3 and below   ║   _T   │
///   ├─────────────────────╫────────┤
///   │ Swift 4             ║  _T0   │
///   ├─────────────────────╫────────┤
///   │ Swift 4.x           ║   $S   │
///   ├─────────────────────╫────────┤
///   │ Swift 5+            ║   $s   │
///   └─────────────────────╨────────┘
///
/// - Parameters:
///   - mangledNameBuffer: A buffer pointer pointing to a null-terminated C
///                        string that contains the mangled Swift symbol.
///   - buffer: A pre-allocated buffer to demangle the Swift symbol as a c-string into.
/// - Returns: An enum, `DemanglingResult`, indicating the various result states
///            of demangling.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(SwiftStdlib 6.3, *)
public func demangle(
  _ mangledNameBuffer: UnsafeBufferPointer<Int8>,
  into buffer: UnsafeMutableBufferPointer<Int8>
) -> DemanglingResult {
  var bufferSize = buffer.count

  let demangledPtr = _swift_runtime_demangle(
    /*mangledName=*/mangledNameBuffer.baseAddress,
    /*mangledNameLength=*/mangledNameBuffer.count - 1,
    /*outputBuffer=*/buffer.baseAddress,
    /*outputBufferSize=*/&bufferSize,
    /*flags=*/0
  )

  guard let demangledPtr else {
    return .invalidSymbol
  }

  // If the buffer size is still equal to the buffer count, the demangle was
  // successful.
  if bufferSize <= buffer.count {
    return .success
  }

  // However if it's not equal, the result was truncated. Return the amount
  // needed to get a full demangle.
  return .truncated(bufferSize)
}

@available(SwiftStdlib 6.3, *)
public func demangle(
  _ mangledNameSpan: Span<UInt8>,
  into buffer: inout MutableSpan<UInt8>
) -> DemanglingResult {
  var outputBufferSize = buffer.count

  let demangledPtr =
  mangledNameSpan.withUnsafeBufferPointer { mangledNameBuffer in
      buffer.withUnsafeMutableBufferPointer { outputBuffer in
        _swift_runtime_demangle(/*mangledName=*/mangledNameBuffer.baseAddress,
          /*mangledNameLength=*/mangledNameBuffer.count,
          /*outputBuffer=*/outputBuffer.baseAddress,
          /*outputBufferSize=*/&outputBufferSize,
          /*flags=*/0)
      }
    }

  guard let demangledPtr else {
    return .invalidSymbol
  }

  // If the buffer size is still equal to the buffer count, the demangle was
  // successful.
  if outputBufferSize <= buffer.count {
    return .success
  }

  // However if it's not equal, the result was truncated. Return the amount
  // needed to get a full demangle.
  return .truncated(outputBufferSize)
}

/// Given a mangled Swift symbol, demangle it into a human readable format.
///
/// Valid Swift symbols begin with the following prefixes:
///   ┌─────────────────────╥────────┐
///   │ Swift Version       ║        │
///   ╞═════════════════════╬════════╡
///   │ Swift 3 and below   ║   _T   │
///   ├─────────────────────╫────────┤
///   │ Swift 4             ║  _T0   │
///   ├─────────────────────╫────────┤
///   │ Swift 4.x           ║   $S   │
///   ├─────────────────────╫────────┤
///   │ Swift 5+            ║   $s   │
///   └─────────────────────╨────────┘
///
/// - Parameters:
///   - mangledName: A mangled Swift symbol.
///   - buffer: A pre-allocated buffer to demangle the Swift symbol as a c-string into.
/// - Returns: An enum, `DemanglingResult`, indicating the various result states
///            of demangling.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(SwiftStdlib 6.3, *)
public func demangle(
  _ mangledName: String,
  into buffer: UnsafeMutableBufferPointer<Int8>
) -> DemanglingResult {
  mangledName.utf8CString.withUnsafeBufferPointer {
    demangle($0, into: buffer)
  }
}

/// Represents whether or not demangling of a symbol was successful.
@available(SwiftStdlib 6.3, *)
public enum DemanglingResult: Equatable {
  /// Demangling completed successfully.
  case success

  /// Demangling resulted in truncating the result. The payload value is the
  /// number of bytes necessary for a full demangle.
  case truncated(Int)

  /// The passed Swift mangled symbol was invalid.
  case invalidSymbol
}
