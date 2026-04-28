/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public class IEnumUnknown: IUnknown {
  override public class var IID: IID { IID_IEnumUnknown }

  public func Clone() throws -> IEnumUnknown {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumUnknown.self, capacity: 1)

    var penum: UnsafeMutablePointer<WinSDK.IEnumUnknown>?
    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Clone(pThis, &penum)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IEnumUnknown(pUnk: penum)
  }

  public func Next(_ celt: ULONG) throws -> [IUnknown] {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumUnknown.self, capacity: 1)

    var rgelt: UnsafeMutablePointer<WinSDK.IUnknown>?
    var celtFetched: ULONG = 0
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.Next(pThis, celt, &rgelt, &celtFetched)
    guard hr == S_OK else { throw COMError(hr: hr) }

    var result: [IUnknown] = []
    result.reserveCapacity(Int(celtFetched))
    _ = rgelt?.withMemoryRebound(to: Optional<UnsafeMutablePointer<WinSDK.IUnknown>>.self,
                                 capacity: Int(celtFetched)) { rgelt in
      for i in 0 ..< Int(celtFetched) {
        result.append(IUnknown(pUnk: rgelt[i]))
      }
    }
    return result
  }

  public func Reset() throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumUnknown.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Reset(pThis)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func Skip(_ celt: ULONG) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumUnknown.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Skip(pThis, celt)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }
}
