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

/// Given a mangled Swift symbol, demangle it into a human readable format.
///
/// If the provided string is not a valid mangled swift identifier this function will throw.
/// If mangling succeeds the returned string will contain a demangled human-readable representation of the identifier.
///
/// - Parameters:
///   - mangledName: A mangled Swift symbol.
/// - Returns: A human readable demangled Swift symbol.
/// - Throws: When the demangling fails for any reason.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(SwiftStdlib 6.3, *)
public func demangle(_ mangledName: String) throws(DemanglingError) -> String {
  var demangledLength: size_t = 0

  let demangled: UnsafeMutablePointer<CChar>? = _swift_runtime_demangle_allocate(
    mangledName, mangledName.utf8.count,
    &demangledLength,
    /*flags=*/0
  )

  guard demangledLength > 0 else {
    assert(demangled == nil)
    throw .invalidSymbol
  }
  defer { free(demangled) }

  return try UnsafeBufferPointer(start: demangled, count: demangledLength)
    .withMemoryRebound(to: UTF8.CodeUnit.self) { (buffer: UnsafeBufferPointer<UTF8.CodeUnit>) throws(DemanglingError) -> String in
      guard let demangledSpan = try? UTF8Span(validating: buffer.span) else {
        throw DemanglingError.invalidSymbol
      }
      return String(copying: demangledSpan)
    }
}

/// Given a mangled Swift symbol, demangle it into a human readable format into the prepared output span.
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
/// - Parameters:
///   - mangledName: A mangled Swift symbol.
///   - output: A pre-allocated span to demangle the Swift symbol into.
/// - Throws: When the demangling failed entirely, and the output span will not have been written to.
/// - Warning: The demangled output is lossy is not not guaranteed to be stable across Swift versions.
///            Future versions of Swift may choose to print more (or less) information in the demangled format.
@available(SwiftStdlib 6.3, *)
public func demangle(
  _ mangledName: borrowing UTF8Span,
  into output: inout OutputSpan<UTF8.CodeUnit>
) throws(DemanglingError) {
  var outputCapacity = output.capacity

  let requiredBufferSize = output.withUnsafeMutableBufferPointer { outputBufferUInt8, initializedOutputLength in
    outputBufferUInt8.withMemoryRebound(to: Int8.self) { outputBuffer in
      mangledName.span.withUnsafeBytes { mangledNamePtr in
        let requiredBufferSize = _swift_runtime_demangle(
          mangledNamePtr.baseAddress, mangledName.count,
          outputBuffer.baseAddress, &outputCapacity,
          /*flags=*/0)

        initializedOutputLength = outputCapacity
        return requiredBufferSize
      }
    }
  }

  guard requiredBufferSize > 0 else {
    throw DemanglingError.invalidSymbol
  }

  // If the buffer size is still equal to the buffer count, the demangle was
  // successful.
  guard requiredBufferSize <= output.capacity else {
    throw DemanglingError.truncated(requiredBufferSize: requiredBufferSize)
  }

  return // OK!
}

/// Error thrown to indicate failure to demangle a Swift symbol.
@available(SwiftStdlib 6.3, *)
public enum DemanglingError: Error {
  /// Demangling resulted in truncating the result. The payload value is the
  /// number of bytes necessary for a full demangle.
  case truncated(requiredBufferSize: Int)

  /// The passed Swift mangled symbol was invalid.
  case invalidSymbol
}
