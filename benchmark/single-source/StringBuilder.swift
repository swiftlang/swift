//===--- StringBuilder.swift ----------------------------------------------===//
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

@inline(never)
func buildString() -> String {
  var sb = "a"
  for str in ["b","c","d","pizza"] {
    sb += str
  }
  return sb
}

@inline(never)
public func run_StringBuilder(_ N: Int) {
  for _ in 1...5000*N {
    _ = buildString()
  }
}

