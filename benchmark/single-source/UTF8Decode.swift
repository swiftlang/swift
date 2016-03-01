//===--- UTF8Decode.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

@inline(never)
public func run_UTF8Decode(N: Int) {
  // Use test data with UTF-8 sequences of all lengths, but mainly ASCII.
  let string = "Mainly ASCII but also some 2-byte sequences (пример),"
    + " some 3-bytes sequences (성능 테스트) and occasionally a 🐼 (4 bytes)"
  let data = Array(string.utf8)
  for _ in 1...N {
    var generator = data.generate()
    var utf8 = UTF8()
    while !utf8.decode(&generator).isEmptyInput() { }
  }
}
