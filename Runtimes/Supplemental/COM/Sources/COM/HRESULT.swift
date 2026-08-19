//===----------------------------------------------------------------------===//
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
public let E_FAIL = HRESULT(bitPattern: 0x80004005)

/// No such interface supported.
public let E_NOINTERFACE = HRESULT(bitPattern: 0x80004002)

/// Not implemented.
public let E_NOTIMPL = HRESULT(bitPattern: 0x80004001)

/// Ran out of memory.
public let E_OUTOFMEMORY = HRESULT(bitPattern: 0x8007000E)

/// One or more arguments are not valid.
public let E_INVALIDARG = HRESULT(bitPattern: 0x80070057)

/// Pointer that is not valid.
public let E_POINTER = HRESULT(bitPattern: 0x80004003)

/// Operation aborted.
public let E_ABORT = HRESULT(bitPattern: 0x80004004)

/// Unexpected failure.
public let E_UNEXPECTED = HRESULT(bitPattern: 0x8000FFFF)

/// General access denied error.
public let E_ACCESSDENIED = HRESULT(bitPattern: 0x80070005)

// MARK: - HRESULT predicates

extension HRESULT {
  /// Whether this HRESULT indicates success (bit 31 is clear).
  @inlinable @_transparent
  public var succeeded: Bool { self >= 0 }

  /// Whether this HRESULT indicates failure (bit 31 is set).
  @inlinable @_transparent
  public var failed: Bool { self < 0 }
}
