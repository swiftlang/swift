/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public class IEnumMoniker: IUnknown {
  override public class var IID: IID { IID_IEnumMoniker }

  public func Clone() throws -> IEnumMoniker {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumMoniker.self, capacity: 1)

    var penum: UnsafeMutablePointer<WinSDK.IEnumMoniker>?
    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Clone(pThis, &penum)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IEnumMoniker(pUnk: penum)
  }

  public func Next(_ celt: ULONG) throws -> [IMoniker] {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumMoniker.self, capacity: 1)

    var rgelt: UnsafeMutablePointer<WinSDK.IMoniker>?
    var celtFetched: ULONG = 0
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.Next(pThis, celt, &rgelt, &celtFetched)
    guard hr == S_OK else { throw COMError(hr: hr) }

    defer { CoTaskMemFree(rgelt) }

    var monikers: [IMoniker] = []
    monikers.reserveCapacity(Int(celtFetched))
    _ = rgelt?.withMemoryRebound(to: UnsafeMutablePointer<WinSDK.IMoniker?>.self,
                                 capacity: Int(celtFetched)) { rgelt in
      for i in 0 ..< Int(celtFetched) {
        monikers.append(IMoniker(pUnk: rgelt[i]))
      }
    }
    return monikers
  }

  public func Reset() throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumMoniker.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Reset(pThis)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func Skip(_ celt: ULONG) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumMoniker.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Skip(pThis, celt)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }
}
