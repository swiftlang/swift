//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if $_MicrosoftCOM || $_CoreFoundationCOM

#if os(Windows)
public import WinSDK
#else
public typealias HRESULT = Int32
#endif

// MARK: - Standard HRESULT constants

/// The operation completed successfully.
public let S_OK = HRESULT(bitPattern: 0)

/// The operation completed successfully with a negative condition
/// (e.g., no more items).
public let S_FALSE = HRESULT(bitPattern: 1)

/// Unspecified failure.
public let E_FAIL = HRESULT(bitPattern: 0x8000_4005)

/// No such interface supported.
public let E_NOINTERFACE = HRESULT(bitPattern: 0x8000_4002)

/// Not implemented.
public let E_NOTIMPL = HRESULT(bitPattern: 0x8000_4001)

/// Ran out of memory.
public let E_OUTOFMEMORY = HRESULT(bitPattern: 0x8007_000E)

/// One or more arguments are not valid.
public let E_INVALIDARG = HRESULT(bitPattern: 0x8007_0057)

/// Pointer that is not valid.
public let E_POINTER = HRESULT(bitPattern: 0x8000_4003)

/// Operation aborted.
public let E_ABORT = HRESULT(bitPattern: 0x8000_4004)

/// Unexpected failure.
public let E_UNEXPECTED = HRESULT(bitPattern: 0x8000_FFFF)

/// General access denied error.
public let E_ACCESSDENIED = HRESULT(bitPattern: 0x8007_0005)

// MARK: - HRESULT predicates

extension HRESULT {
  /// Whether this HRESULT indicates success (bit 31 is clear).
  @inlinable @_transparent
  public var succeeded: Bool { self >= 0 }

  /// Whether this HRESULT indicates failure (bit 31 is set).
  @inlinable @_transparent
  public var failed: Bool { self < 0 }
}

@_transparent
@usableFromInline
package func SUCCEEDED(_ hr: HRESULT) -> Bool {
  return hr.succeeded
}

@_transparent
@usableFromInline
package func FAILED(_ hr: HRESULT) -> Bool {
  return hr.failed
}

#endif // $_MicrosoftCOM || $_CoreFoundationCOM
