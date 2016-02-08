//===--- StringTests.swift ------------------------------------------------===//
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

public func run_StringWithCString(N: Int) {
  let str = String(count:100 * (1 << 16), repeatedValue:"x" as UnicodeScalar)
  for _ in 0 ..< N {
    str.withCString { _ in }
  }
}
