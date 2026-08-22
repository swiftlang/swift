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

/// Rich error information for a failed COM `HRESULT`.
///
/// `IErrorInfo` is the standard COM interface for providing human-readable
/// error details alongside a failing `HRESULT`. When a COM method fails, the
/// synthesised wrapper captures the thread-local `IErrorInfo` into a
/// `COMError`.
///
/// Classes that opt into `ISupportErrorInfo` must also conform to `IErrorInfo`
/// to provide the error detail fields that `ISupportErrorInfo` advertises.
@com(interface: "1CF2B120-547D-101B-8E65-08002B2BD119")
public protocol IErrorInfo: IUnknown {
  /// The GUID of the interface that defined the error.
  func GetGUID(_ pGUID: UnsafeMutablePointer<WinSDK.GUID>?) -> HRESULT

  /// The programmatic identifier of the component that raised the error.
  func GetSource(_ pBstrSource: UnsafeMutablePointer<BSTR?>?) -> HRESULT

  /// A human-readable description of the error.
  func GetDescription(_ pBstrDescription: UnsafeMutablePointer<BSTR?>?) -> HRESULT

  /// The path to the help file that describes the error.
  func GetHelpFile(_ pBstrHelpFile: UnsafeMutablePointer<BSTR?>?) -> HRESULT

  /// The help context identifier within the help file.
  func GetHelpContext(_ pdwHelpContext: UnsafeMutablePointer<DWORD>?) -> HRESULT
}

extension IErrorInfo {
  internal var guid: WinSDK.GUID {
    get throws {
      var iid: WinSDK.GUID = GUID_NULL
      let hr = GetGUID(&iid)
      guard hr.succeeded, iid != GUID_NULL else {
        throw COMError(hr: hr)
      }
      return iid
    }
  }

  internal var source: String {
    get throws {
      var bstr: BSTR?
      let hr = GetSource(&bstr)
      guard hr.succeeded, let source = String(consuming: bstr) else {
        throw COMError(hr: hr)
      }
      return source
    }
  }

  internal var description: String {
    get throws {
      var bstr: BSTR?
      let hr = GetDescription(&bstr)
      guard hr.succeeded, let description = String(consuming: bstr) else {
        throw COMError(hr: hr)
      }
      return description
    }
  }

  internal var helpFile: String {
    get throws {
      var bstr: BSTR?
      let hr = GetHelpFile(&bstr)
      guard hr.succeeded, let helpFile = String(consuming: bstr) else {
        throw COMError(hr: hr)
      }
      return helpFile
    }
  }

  internal var helpContext: DWORD {
    get throws {
      var dwContext: DWORD = .max
      let hr = GetHelpContext(&dwContext)
      guard hr.succeeded else {
        throw COMError(hr: hr)
      }
      return dwContext
    }
  }
}

#endif
