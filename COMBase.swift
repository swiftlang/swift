/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public func CoInitializeEx(_ pvReserved: LPVOID?,
                           _ dwCoInit: COINIT) -> HRESULT {
  return WinSDK.CoInitializeEx(pvReserved, DWORD(dwCoInit.rawValue))
}

public func CoInitializeEx(_ dwCoInit: COINIT = COINIT_MULTITHREADED) throws {
  // MSDN: pvReserved: This parameter is reserved and must be NULL.
  let hr: HRESULT = CoInitializeEx(nil, dwCoInit)
  switch hr {
  case S_OK, S_FALSE, RPC_E_CHANGED_MODE: break
  default: throw COMError(hr: hr)
  }
}

public func CoGetMalloc() throws -> IMalloc {
  var pMalloc: LPMALLOC?
  // MSDN: dwMemContext: This parameter must be 1.
  let hr: HRESULT = CoGetMalloc(1, &pMalloc)
  guard hr == S_OK else { throw COMError(hr: hr) }
  return IMalloc(pUnk: pMalloc)
}
