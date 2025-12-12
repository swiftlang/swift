//===--- Mangling.swift --------------------------------------*- swift -*-===//
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

/// Given a mangled Swift, or C++, symbol, demangle it into a human readable format.
///
/// If the provided bytes are not a valid mangled swift name, the output span will be initialized with zero elements.
/// If mangling succeeds the output span will contain the resulting demangled string.
/// A successfully demangled string is _not_ null terminated, and its length is communicated by the `initializedCount`
/// of the output span.
///
/// The demangled output may be _truncated_ if the output span's capacity is insufficient for the
/// demangled output string! You can detect this situation by inspecting the returned ``DemanglingResult``,
/// for the ``DemanglingResult/truncated`` case.
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
/// This function also attempts to demangle C++ symbols, where possible.
///
/// - Parameters:
///   - mangledName: A mangled Swift symbol.
/// - Returns: A human readable demangled Swift symbol.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(StdlibDeploymentTarget 6.3, *)
public func demangle(_ mangledName: String) -> String? {
  var length: size_t = 0

  let demangled: UnsafeMutablePointer<CChar>? = _swift_runtime_demangle(
    mangledName, mangledName.utf8.count,
    nil, &length,
    /*flags=*/0
  )

  guard length > 0 else {
    assert(demangled == nil)
    return nil
  }
  defer { free(demangled) }

  return UnsafeBufferPointer(start: demangled, count: length).withMemoryRebound(to: UTF8.CodeUnit.self) { buffer in 
    guard let demangledSpan = try? UTF8Span(validating: buffer.span) else {
      return nil
    }
    return String(copying: demangledSpan)
  }
}

/// Given a mangled Swift symbol, demangle it into a human readable format.
///
/// If the provided bytes are not a valid mangled swift name, the output span will be initialized with zero elements.
/// If mangling succeeds the output span will contain the resulting demangled string.
/// A successfully demangled string is _not_ null terminated, and its length is communicated by the `initializedCount`
/// of the output span.
/// 
/// The demangled output may be _truncated_ if the output span's capacity is insufficient for the
/// demangled output string! You can detect this situation by inspecting the returned ``DemanglingResult``,
/// for the ``DemanglingResult/truncated`` case.
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
///   - mangledName: Mangled name to be demangled.
///   - output: A pre-allocated span to demangle the Swift symbol into.
/// - Returns: An enum, `DemanglingResult`, indicating the various result states
///            of demangling.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(StdlibDeploymentTarget 6.3, *)
public func demangle(
  _ mangledName: borrowing UTF8Span,
  into output: inout OutputSpan<UTF8.CodeUnit>
) -> DemanglingResult {
  var demangledLength = output.capacity
  let outputCapacity = output.capacity

  let demangledPtr = output.withUnsafeMutableBufferPointer { outputBufferUInt8, outputLength in 
    outputBufferUInt8.withMemoryRebound(to: Int8.self) { outputBuffer in 
      mangledName.span.withUnsafeBytes { mangledNamePtr in
        let res = _swift_runtime_demangle(
          mangledNamePtr.baseAddress, mangledName.count,
          outputBuffer.baseAddress, &demangledLength,
          /*flags=*/0)

        // If the demangled string is longer than the Span capacity, we 
        // demangled only up-to the capacity of the span, and therefore must 
        // indicate this here. Such situation will return a `.truncated` result.
        outputLength = min(outputCapacity, demangledLength)

        return res
      }
    }
  }

  guard demangledPtr != nil else {
    return .invalidSymbol
  }

  // If the buffer size is still equal to the buffer count, the demangle was
  // successful.
  if demangledLength <= output.capacity {
    return .success
  }

  // The result was truncated. Return the amount needed to get a full demangle.
  return .truncated(demangledLength)
}

/// Represents whether or not demangling of a symbol was successful.
@available(StdlibDeploymentTarget 6.3, *)
public enum DemanglingResult: Equatable {
  /// Demangling completed successfully.
  case success

  /// Demangling resulted in truncating the result. The payload value is the
  /// number of bytes necessary for a full demangle.
  case truncated(Int)

  /// The passed Swift mangled symbol was invalid.
  case invalidSymbol
}
