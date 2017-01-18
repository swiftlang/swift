//===--- NSError.swift ----------------------------------------------------===//
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
import Foundation

protocol P {
  func buzz() throws -> Int
}

class K : P {
  init() {}
  func buzz() throws -> Int {
    throw NSError(domain: "AnDomain", code: 42, userInfo: nil)
  }
}

class G : K {
  override init() {}
  override func buzz() throws -> Int { return 0 }
}

func caller(_ x: P) throws {
  _ = try x.buzz()
}

@inline(never)
public func run_NSError(_ N: Int) {
  for _ in 1...N*1000 {
      let k = K()
      let g = G()
      do {
        try caller(g)
        try caller(k)
      } catch _ {
        continue
      }
  }
}
