/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public class IBindCtx: IUnknown {
  override public class var IID: IID { IID_IBindCtx }

  public func EnumObjectParam() throws -> IEnumString {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var penum: UnsafeMutablePointer<WinSDK.IEnumString>?
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.EnumObjectParam(pThis, &penum)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IEnumString(pUnk: penum)
  }

  public func GetBindOptions() throws -> BIND_OPTS {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var bindopts: BIND_OPTS = BIND_OPTS()
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.GetBindOptions(pThis, &bindopts)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return bindopts
  }

  public func GetObjectParam(_ pszKey: String) throws -> IUnknown {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var punk: UnsafeMutablePointer<WinSDK.IUnknown>?
    var key: [OLECHAR] = pszKey.wide
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.GetObjectParam(pThis, &key, &punk)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IUnknown(pUnk: punk)
  }

  public func GetRunningObjectTable() throws -> IRunningObjectTable {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var prot: UnsafeMutablePointer<WinSDK.IRunningObjectTable>?
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.GetRunningObjectTable(pThis, &prot)
    guard hr == S_OK else { throw COMError(hr: hr) }
    return IRunningObjectTable(pUnk: prot)
  }

  public func RegisterObjectBound(_ punk: IUnknown) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.RegisterObjectBound(pThis, punk.pUnk)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func RegisterObjectParam(_ pszKey: String, _ punk: IUnknown)
      throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var key: [OLECHAR] = pszKey.wide
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.RegisterObjectParam(pThis, &key, punk.pUnk)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func ReleaseBoundObjects() throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    let hr: HRESULT = pThis.pointee.lpVtbl.pointee.ReleaseBoundObjects(pThis)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func RevokeObjectBound(_ punk: IUnknown) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.RevokeObjectBound(pThis, punk.pUnk)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func RevokeObjectParam(_ pszKey: String) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    var key: [OLECHAR] = pszKey.wide
    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.RevokeObjectParam(pThis, &key)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }

  public func SetBindOptions(_ pbindopts: inout BIND_OPTS) throws {
    guard let pUnk = UnsafeMutableRawPointer(self.pUnk) else {
      throw COMError(hr: E_INVALIDARG)
    }
    let pThis = pUnk.bindMemory(to: WinSDK.IBindCtx.self, capacity: 1)

    let hr: HRESULT =
        pThis.pointee.lpVtbl.pointee.SetBindOptions(pThis, &pbindopts)
    guard hr == S_OK else { throw COMError(hr: hr) }
  }
}
