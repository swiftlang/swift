//===--- StringInterpolation.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

class RefTypePrintable : CustomStringConvertible {
  var description: String {
    return "01234567890123456789012345678901234567890123456789"
  }
}

@inline(never)
public func run_StringInterpolation(_ N: Int) {
  let reps = 100
  let refResult = reps
  let anInt: Int64 = 0x1234567812345678
  let aRefCountedObject = RefTypePrintable()

  for _ in 1...100*N {
    var result = 0
    for _ in 1...reps {
      let s = "\(anInt) abcdefdhijklmn \(aRefCountedObject) abcdefdhijklmn \u{01}"
      let utf16 = s.utf16

      // FIXME: if String is not stored as UTF-16 on this platform, then the
      // following operation has a non-trivial cost and needs to be replaced
      // with an operation on the native storage type.
      result = result &+ Int(utf16[utf16.index(before: utf16.endIndex)])
    }
    CheckResults(result == refResult, "IncorrectResults in StringInterpolation: \(result) != \(refResult)")
  }
}

