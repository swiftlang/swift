//===--- EightByteBuffer.swift --------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  A statically allocated buffer for holding a small number of bytes.
//
//===----------------------------------------------------------------------===//

import Swift

struct EightByteBuffer {
  var word: UInt64

  init() {
    word = 0
  }

  init(_ qword: UInt64) {
    word = qword.bigEndian
  }

  init(_ qword: Int64) {
    self.init(UInt64(bitPattern: qword))
  }

  init<T: FixedWidthInteger>(_ value: T) where T: SignedInteger {
    self.init(Int64(value))
  }

  init<T: FixedWidthInteger>(_ value: T) {
    self.init(UInt64(value))
  }

  subscript(ndx: Int) -> UInt8 {
    get {
      if ndx < 0 || ndx >= 8 {
        fatalError("Index out of range")
      }
      return withUnsafeBytes(of: word) { buffer in
        return buffer[ndx]
      }
    }
    set(newValue) {
      if ndx < 0 || ndx >= 8 {
        fatalError("Index out of range")
      }
      withUnsafeMutableBytes(of: &word) { buffer in
        buffer[ndx] = newValue
      }
    }
  }
}
