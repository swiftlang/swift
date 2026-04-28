/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public class IEnumString: IUnknown {
  override public class var IID: IID { IID_IEnumString }

  public func Clone() throws -> IEnumString {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumString.self, capacity: 1)

    var penum: UnsafeMutablePointer<WinSDK.IEnumString>?
    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Clone(pThis, &penum)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IEnumString(pUnk: penum)
  }

  public func Next(_ celt: ULONG) throws -> [String] {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumString.self, capacity: 1)

    var rgelt: LPOLESTR?
    var celtFetched: ULONG = 0
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.Next(pThis, celt, &rgelt, &celtFetched)
    guard hr == S_OK else { throw COMError(hr: hr) }

    defer { CoTaskMemFree(rgelt) }

    var result: [String] = []
    result.reserveCapacity(Int(celtFetched))
    _ = rgelt?.withMemoryRebound(to: LPOLESTR?.self,
                                 capacity: Int(celtFetched)) { rgelt in
      for i in 0 ..< Int(celtFetched) {
        result.append(String(decodingCString: rgelt[i]!, as: UTF16.self))
      }
    }
    return result
  }

  public func Reset() throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumString.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Reset(pThis)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func Skip(_ celt: ULONG) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IEnumString.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.Skip(pThis, celt)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }
}
