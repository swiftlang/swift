//===--- CxxStringConversion.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
import CxxStdlibPerformance
import CxxStdlib

let cxxStringSize = 1_000_000
let swiftStringSize = 1_000_000

var cxxString: std.string? = nil
var swiftString: String? = nil

public let benchmarks = [
  BenchmarkInfo(
    name: "CxxStringConversion.swift.to.cxx",
    runFunction: run_swiftToCxx,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: {
      swiftString = String(repeating: "abc012", count: swiftStringSize / 6)
    }),
  BenchmarkInfo(
    name: "CxxStringConversion.cxx.to.swift",
    runFunction: run_cxxToSwift,
    tags: [.validation, .bridging, .cxxInterop],
    setUpFunction: {
      cxxString = std.string()
      for i in 0..<cxxStringSize {
        let char = std.string.value_type(65 + i % 10) // latin letters A-J
        cxxString!.push_back(char)
      }
    }),
]

@inline(never)
public func run_swiftToCxx(_ n: Int) {
  let str = swiftString!
  for _ in 0..<n {
    let x = std.string(str)
    blackHole(x)
  }
}

@inline(never)
public func run_cxxToSwift(_ n: Int) {
  let str = cxxString!
  for _ in 0..<n {
    let x = String(str)
    blackHole(x)
  }
}
