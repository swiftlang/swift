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

internal import WinSDK

extension String {
  internal init?(_ bstr: BSTR?) {
    guard let bstr else { return nil }
    self = String(decoding: UnsafeBufferPointer(start: bstr,
                                                count: Int(SysStringLen(bstr))),
                  as: UTF16.self)
  }

  internal init?(consuming bstr: consuming BSTR?) {
    guard let bstr else { return nil }
    defer { SysFreeString(bstr) }
    self = String(decoding: UnsafeBufferPointer(start: bstr,
                                                count: Int(SysStringLen(bstr))),
                  as: UTF16.self)
  }
}

extension BSTR {
  internal init(_ string: borrowing String) {
    self = Array(string.utf16).withUnsafeBufferPointer {
      SysAllocStringLen($0.baseAddress, UINT($0.count))
    }
  }
}

#endif
