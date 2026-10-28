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

extension String {
  @usableFromInline
  @_alwaysEmitIntoClient
  internal init?(_ bstr: BSTR?) {
    guard let bstr else { return nil }
    self = String(decoding: UnsafeBufferPointer(start: bstr,
                                                count: Int(SysStringLen(bstr))),
                  as: UTF16.self)
  }

  @usableFromInline
  @_alwaysEmitIntoClient
  internal init?(consuming bstr: consuming BSTR?) {
    guard let bstr else { return nil }
    defer { SysFreeString(bstr) }
    self = String(decoding: UnsafeBufferPointer(start: bstr,
                                                count: Int(SysStringLen(bstr))),
                  as: UTF16.self)
  }
}

extension BSTR {
  @usableFromInline
  @_alwaysEmitIntoClient
  internal init(_ string: borrowing String) {
    self = string.withCString(encodedAs: UTF16.self) { buffer in
      SysAllocStringLen(buffer, UINT(string.utf16.count))
    }
  }
}

#endif
