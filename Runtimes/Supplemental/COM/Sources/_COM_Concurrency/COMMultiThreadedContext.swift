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

internal import COM
private import WinSDK

@usableFromInline
internal struct COMMultiThreadedContext: ~Copyable, @unchecked Sendable {
  private var cookie: CO_MTA_USAGE_COOKIE

  @usableFromInline
  internal init() throws(COMError) {
    var cookie: CO_MTA_USAGE_COOKIE?
    let hr = CoIncrementMTAUsage(&cookie)
    guard SUCCEEDED(hr), let cookie else {
      throw COMError(hr: hr)
    }
    self.cookie = cookie
  }

  deinit {
    let hr = CoDecrementMTAUsage(cookie)
    precondition(SUCCEEDED(hr))
  }
}

#endif // $_MicrosoftCOM
