/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import WinSDK

public struct COMError: Error {
  public let hr: HRESULT

  public init(hr: HRESULT) {
    self.hr = hr
  }
}

extension COMError: CustomStringConvertible {
  public var description: String {
    let dwFlags: DWORD = DWORD(FORMAT_MESSAGE_ALLOCATE_BUFFER)
                       | DWORD(FORMAT_MESSAGE_FROM_SYSTEM)
                       | DWORD(FORMAT_MESSAGE_IGNORE_INSERTS)

    var buffer: UnsafeMutablePointer<WCHAR>? = nil
    let dwResult: DWORD = withUnsafeMutablePointer(to: &buffer) {
      $0.withMemoryRebound(to: WCHAR.self, capacity: 2) {
        FormatMessageW(dwFlags, nil, DWORD(bitPattern: hr),
                       MAKELANGID(WORD(LANG_NEUTRAL), WORD(SUBLANG_DEFAULT)),
                       $0, 0, nil)
      }
    }
    guard dwResult > 0, let message = buffer else {
      return "HRESULT(0x\(String(DWORD(bitPattern: hr), radix: 16)))"
    }
    defer { LocalFree(buffer) }
    return "0x\(String(DWORD(bitPattern: hr), radix: 16)) - \(String(decodingCString: message, as: UTF16.self))"
  }
}
