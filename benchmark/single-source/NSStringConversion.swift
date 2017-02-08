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

public func run_NSStringConversion(_ N: Int) {
let test:NSString = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)!
  for _ in 1...N * 10000 {
    _ = test as String
  }
}
