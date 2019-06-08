//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/// Represents the potential return types from a call to demangle.
public enum DemangleResult: Equatable {
  /// The demangle completed successfully.
  case success

  /// The demangle resulted in truncating the result. The payload value is the
  /// number of bytes necessary for a full demangle.
  case truncated(Int)

  /// The passed Swift mangled symbol was invalid.
  case invalidSymbol
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
/// - Returns: A human readable demangled Swift symbol.
public func demangle(_ mangledName: String) -> String? {
  return mangledName.utf8CString.withUnsafeBufferPointer {
    let demangledPtr = _swift_stdlib_demangle(
      /* mangledName */ $0.baseAddress,
      /* mangledNameLength */ $0.count - 1,
      /* outputBuffer */ nil,
      /* outputBufferSize */ nil,
      /* flags */ 0
    )

    guard demangledPtr != nil else {
      return nil
    }

    let demangledName = String(cString: demangledPtr!)
    _swift_stdlib_free(demangledPtr!)
    return demangledName
  }
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
///   - mangledNameBuffer: A buffer pointer pointing to a null-terminated C
///                        string that contains the mangled Swift symbol.
///   - buffer: A pre-allocated buffer to demangle the Swift symbol into.
/// - Returns: An enum, `DemangleResult`, indicating the various result states
///            of demangling.
public func demangle(
  _ mangledNameBuffer: UnsafeBufferPointer<Int8>,
  into buffer: UnsafeMutableBufferPointer<Int8>
) -> DemangleResult {
  var bufferSize = buffer.count

  let demangledPtr = _swift_stdlib_demangle(
    /* mangledName */ mangledNameBuffer.baseAddress,
    /* mangledNameLength */ mangledNameBuffer.count - 1,
    /* outputBuffer */ buffer.baseAddress,
    /* outputBufferSize */ &bufferSize,
    /* flags */ 0
  )

  guard demangledPtr != nil else {
    return .invalidSymbol
  }

  // If the buffer size is still equal to the buffer count, the demangle was
  // successful.
  if bufferSize == buffer.count {
    return .success
  }

  // However if it's not equal, the result was truncated. Return the amount
  // needed to get a full demangle.
  return .truncated(bufferSize)
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
///   - buffer: A pre-allocated buffer to demangle the Swift symbol into.
/// - Returns: An enum, `DemangleResult`, indicating the various result states
///            of demangling.
public func demangle(
  _ mangledName: String,
  into buffer: UnsafeMutableBufferPointer<Int8>
) -> DemangleResult {
  mangledName.utf8CString.withUnsafeBufferPointer {
    demangle($0, into: buffer)
  }
}
