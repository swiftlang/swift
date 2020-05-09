//===--- SevenBoom.swift --------------------------------------------------===//
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

// 15% _swift_allocObject (String.bridgeToObjectiveC)
// 14% [NSError dealloc]
// 14% objc_allocWithZone
// 10% _swift_allocObject
// 11% _swift_release_dealloc
//  8% objc_release
//  7% objc_msgSend
//  5% _swift_release_
//  2% _swift_retain_
public let SevenBoom = BenchmarkInfo(
  name: "SevenBoom",
  runFunction: run_SevenBoom,
  tags: [.runtime, .exceptions, .bridging, .cpubench]
)

@inline(never)
func filter_seven(_ input : Int) throws {
  guard case 7 = input else {
    throw NSError(domain: "AnDomain", code: 42, userInfo: nil)
  }
}

@inline(never)
public func run_SevenBoom(_ N: Int) {
  var c = 0
  for i in 1...N*5000 {
    do {
      try filter_seven(i)
      c += 1
    }
    catch _ {
    }
  }
  CheckResults(c == 1)
}

