//===--- StringEnum.swift -------------------------------------------------===//
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

public let StringEnum = BenchmarkInfo(
  name: "StringSwitch",
  runFunction: run_StringSwitch,
  tags: [.validation, .api, .String],
  legacyFactor: 20)

@inline(never)
func test(_ s: String) {
    switch s {
        case "Swift": return "Swift"
        case "foo": return "foo"
        case "bar": return "bar"
        case "(C, C++, Objective-C).": return "(C, C++, Objective-C)."
        case "baz": return "baz"
        case "biz": return "biz"
    }
}

@inline(never)
public func run_StringSwitch(_ N: Int) {
  let first = "Swift"
  let short = "foo"
  let long = "(C, C++, Objective-C)."
  let last = "biz"
  for _ in 1...100*N {
      blackHole(test(first))
      blackHole(test(short))
      blackHole(test(long))
      blackHole(test(last))
  }
}
