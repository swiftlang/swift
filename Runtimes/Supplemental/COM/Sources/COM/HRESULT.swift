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

#if $_MicrosoftCOM

public import WinSDK

// WinSDK defines these constants through `_HRESULT_TYPEDEF_`, which the Clang
// importer cannot represent. Preserve their canonical spellings in Swift.

// MARK: - Standard HRESULT constants

/// The operation completed successfully.
@_transparent
public var S_OK: HRESULT {
  HRESULT(bitPattern: 0)
}

/// The operation completed successfully with a negative condition
/// (e.g., no more items).
@_transparent
public var S_FALSE: HRESULT {
  HRESULT(bitPattern: 1)
}

/// Unspecified failure.
@_transparent
public var E_FAIL: HRESULT {
  HRESULT(bitPattern: 0x8000_4005)
}

/// No such interface supported.
@_transparent
public var E_NOINTERFACE: HRESULT {
  HRESULT(bitPattern: 0x8000_4002)
}

/// Not implemented.
@_transparent
public var E_NOTIMPL: HRESULT {
  HRESULT(bitPattern: 0x8000_4001)
}

/// Ran out of memory.
@_transparent
public var E_OUTOFMEMORY: HRESULT {
  HRESULT(bitPattern: 0x8007_000E)
}

/// One or more arguments are not valid.
@_transparent
public var E_INVALIDARG: HRESULT {
  HRESULT(bitPattern: 0x8007_0057)
}

/// Pointer that is not valid.
@_transparent
public var E_POINTER: HRESULT {
  HRESULT(bitPattern: 0x8000_4003)
}

/// Operation aborted.
@_transparent
public var E_ABORT: HRESULT {
  HRESULT(bitPattern: 0x8000_4004)
}

/// Unexpected failure.
@_transparent
public var E_UNEXPECTED: HRESULT {
  HRESULT(bitPattern: 0x8000_FFFF)
}

/// General access denied error.
@_transparent
public var E_ACCESSDENIED: HRESULT {
  HRESULT(bitPattern: 0x8007_0005)
}

// MARK: - HRESULT predicates

@_transparent
@usableFromInline
package func SUCCEEDED(_ hr: HRESULT) -> Bool {
  return hr >= 0
}

@_transparent
@usableFromInline
package func FAILED(_ hr: HRESULT) -> Bool {
  return hr < 0
}

#endif // $_MicrosoftCOM
