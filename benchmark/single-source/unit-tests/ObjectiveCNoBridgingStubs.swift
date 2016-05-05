//===--- ObjectiveCNoBridgingStubs.swift ----------------------------------===//
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
//
// This file is compiled with -Xfrontend -disable-swift-bridge-attr. No bridging
// of swift types happens.
//
//===----------------------------------------------------------------------===//

import TestsUtils
import Foundation
import ObjectiveCTests

@inline(never)
func testObjectiveCBridgeStubFromNSStringRef() {
  let b = BridgeTester()
  var nsString : NSString = NSString()
  for _ in 0 ..< 10_000 {
    nsString = b.testToString()
  }
  CheckResults(nsString.isEqual(to: "Default string value no tagged pointer" as NSString), "Wrong value returned")
}

@inline(never)
public func run_ObjectiveCBridgeStubFromNSStringRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubFromNSStringRef()
    }
  }
}
