/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

// FIXME(compnerd) unfortunately these must be public as they are part of the
// public API.  These really should be vended by WinSDK.
public typealias REFIID = UnsafePointer<IID>
public typealias REFGUID = UnsafePointer<GUID>
public typealias REFCLSID = UnsafePointer<CLSID>
public typealias REFPROPVARIANT = UnsafePointer<PROPVARIANT>
public typealias REFPROPERTYKEY = UnsafePointer<PROPERTYKEY>

public typealias REFWICPixelFormatGUID = UnsafePointer<WICPixelFormatGUID>

// winerror.h
internal var E_INVALIDARG: HRESULT {
  HRESULT(bitPattern: 0x80070057)
}

internal var E_NOINTERFACE: HRESULT {
  HRESULT(bitPattern: 0x80004002)
}

internal var E_FAIL: HRESULT {
  HRESULT(bitPattern: 0x80004005)
}

internal var RPC_E_CHANGED_MODE: HRESULT {
  HRESULT(bitPattern: 0x80010106)
}

// winnt.h
@_transparent
public func MAKELANGID(_ p: WORD, _ s: WORD) -> DWORD {
  return DWORD((s << 10) | p)
}
