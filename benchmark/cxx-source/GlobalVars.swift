//===--- GlobalVars.swift -------------------------------------------------===//
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

// This is a simple test that creates thousands of C++ objects and does nothing
// with them.

import TestsUtils
import CxxGlobalVars

public let GlobalVars = BenchmarkInfo(
  name: "GlobalVars",
  runFunction: run_GlobalVars,
  tags: [.validation, .bridging])

@inline(never)
public func run_GlobalVars(_ N: Int) {
  for i in 0...(N * 10_000) {
    let localVar1 = globalInt
    let localVar2 = globalConstexprInt
    let localVar3 = globalFloat
    let localVar4 = globalConstexprFloat
    let localVar5 = globalBigObject
    let localVar6 = globalConstexprBigObject
    blackHole(localVar1)
    blackHole(localVar2)
    blackHole(localVar3)
    blackHole(localVar4)
    blackHole(localVar5)
    blackHole(localVar6)
  }
}
