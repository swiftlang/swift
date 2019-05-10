//===--- DictionaryLiteral.swift ------------------------------------------===//
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

// Dictionary creation from literals benchmark
// rdar://problem/19804127
import TestsUtils

public let DictionaryLiteral = BenchmarkInfo(
  name: "DictionaryLiteral",
  runFunction: run_DictionaryLiteral,
  tags: [.validation, .api, .Dictionary],
  legacyFactor: 10)

@inline(never)
func makeDictionary() -> [Int: Int] {
  return [1: 3, 2: 2, 3: 1]
}

@inline(never)
public func run_DictionaryLiteral(_ N: Int) {
  for _ in 1...1000*N {
    _ = makeDictionary()
  }
}
