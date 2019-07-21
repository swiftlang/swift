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
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)

import TestsUtils
import Foundation

fileprivate var test:NSString = ""

public let NSStringConversion = [
  BenchmarkInfo(name: "NSStringConversion",
                runFunction: run_NSStringConversion,
                tags: [.validation, .api, .String, .bridging]),
  BenchmarkInfo(name: "NSStringConversion.UTF8",
                runFunction: run_NSStringConversion_nonASCII,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "tëst", encoding: String.Encoding.utf8.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Mutable",
                runFunction: run_NSMutableStringConversion,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSMutableString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Medium",
                runFunction: run_NSStringConversion_medium,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "aaaaaaaaaaaaaaa", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Long",
                runFunction: run_NSStringConversion_long,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "The quick brown fox jumps over the lazy dog", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCII,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "Thë qüick bröwn föx jumps over the lazy dög", encoding: String.Encoding.utf8.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge",
                runFunction: run_NSStringConversion_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.UTF8",
                runFunction: run_NSStringConversion_nonASCII_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "tëst", encoding: String.Encoding.utf8.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Mutable",
                runFunction: run_NSMutableStringConversion_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSMutableString(cString: "test", encoding: String.Encoding.ascii.rawValue)! }),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Medium",
                runFunction: run_NSStringConversion_medium_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "aaaaaaaaaaaaaaa", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.Long",
                runFunction: run_NSStringConversion_long_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "The quick brown fox jumps over the lazy dog", encoding: String.Encoding.ascii.rawValue)! } ),
  BenchmarkInfo(name: "NSStringConversion.Rebridge.LongUTF8",
                runFunction: run_NSStringConversion_longNonASCII_rebridge,
                tags: [.validation, .api, .String, .bridging],
                setUpFunction: { test = NSString(cString: "Thë qüick bröwn föx jumps over the lazy dög", encoding: String.Encoding.utf8.rawValue)! })]

public func run_NSStringConversion(_ N: Int) {
let test:NSString = NSString(cString: "test", encoding: String.Encoding.ascii.rawValue)!
  for _ in 1...N * 10000 {
    //Doesn't test accessing the String contents to avoid changing historical benchmark numbers
    blackHole(identity(test) as String)
  }
}

fileprivate func innerLoop(_ str: NSString, _ N: Int, _ scale: Int = 5000) {
  for _ in 1...N * scale {
    for char in (identity(str) as String).utf8 {
      blackHole(char)
    }
  }
}

public func run_NSStringConversion_nonASCII(_ N: Int) {
  innerLoop(test, N, 2500)
}

public func run_NSMutableStringConversion(_ N: Int) {
  innerLoop(test, N)
}

public func run_NSStringConversion_medium(_ N: Int) {
  innerLoop(test, N, 1000)
}

public func run_NSStringConversion_long(_ N: Int) {
  innerLoop(test, N, 1000)
}

public func run_NSStringConversion_longNonASCII(_ N: Int) {
  innerLoop(test, N, 300)
}

fileprivate func innerRebridge(_ str: NSString, _ N: Int, _ scale: Int = 5000) {
  for _ in 1...N * scale {
    let bridged = identity(str) as String
    blackHole(bridged)
    blackHole(bridged as NSString)
  }
}

public func run_NSStringConversion_rebridge(_ N: Int) {
  innerRebridge(test, N, 2500)
}

public func run_NSStringConversion_nonASCII_rebridge(_ N: Int) {
  innerRebridge(test, N, 2500)
}

public func run_NSMutableStringConversion_rebridge(_ N: Int) {
  innerRebridge(test, N)
}

public func run_NSStringConversion_medium_rebridge(_ N: Int) {
  innerRebridge(test, N, 1000)
}

public func run_NSStringConversion_long_rebridge(_ N: Int) {
  innerRebridge(test, N, 1000)
}

public func run_NSStringConversion_longNonASCII_rebridge(_ N: Int) {
  innerRebridge(test, N, 300)
}

#endif
