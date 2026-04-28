/**
 * Copyright 2020 Saleem Abdulrasool <compnerd@compnerd.org>
 * All Rights Reserved.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 **/

import ucrt
import WinSDK

extension Array where Array.Element == WCHAR {
  internal init(from string: String) {
    self = string.withCString(encodedAs: UTF16.self) { buffer in
      Array<WCHAR>(unsafeUninitializedCapacity: string.utf16.count + 1) {
        wcscpy_s($0.baseAddress, $0.count, buffer)
        $1 = $0.count
      }
    }
  }
}

extension String {
  init(from wide: [WCHAR]) {
    self = wide.withUnsafeBufferPointer {
      String(decodingCString: $0.baseAddress!, as: UTF16.self)
    }
  }
}

extension String {
  internal var wide: [WCHAR] {
    return Array<WCHAR>(from: self)
  }
}
