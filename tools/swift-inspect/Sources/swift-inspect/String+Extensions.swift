//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftRemoteMirror
import Foundation

extension DefaultStringInterpolation {
  mutating func appendInterpolation<T>(hex: T) where T: BinaryInteger {
    appendInterpolation("0x")
    appendInterpolation(String(hex, radix: 16))
  }
}

enum Std {
  struct File: TextOutputStream {

#if os(Android)
    typealias File = OpaquePointer
#else
    typealias File = UnsafeMutablePointer<FILE>
#endif

    var underlying: File

    mutating func write(_ string: String) {
      fputs(string, underlying)
    }
  }

  static var err = File(underlying: stderr)
}

internal func disableStdErrBuffer() {
  setvbuf(stderr, nil, Int32(_IONBF), 0)
}
