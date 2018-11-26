//===--- NSStringConversion.swift -----------------------------------------===//
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

// <rdar://problem/19003201>
import TestsUtils
import Foundation

public let NSStringConversion = BenchmarkInfo(
  name: "NSStringConversion",
  runFunction: run_NSStringConversion,
  tags: [.validation, .api, .String, .bridging])

public func run_NSStringConversion(_ N: Int) {
#if _runtime(_ObjC)
let test:NSString = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)!
  for _ in 1...N * 10000 {
    blackHole(identity(test) as String)
  }
#endif
}
