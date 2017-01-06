//===--- ObjectiveCNoBridgingStubs.swift ----------------------------------===//
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

@inline(never)
func testObjectiveCBridgeStubToNSStringRef() {
   let b = BridgeTester()
   let str = NSString(cString: "hello world", encoding: String.Encoding.utf8.rawValue)!
   for _ in 0 ..< 10_000 {
     b.test(from: str)
   }
}

@inline(never)
public func run_ObjectiveCBridgeStubToNSStringRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubToNSStringRef()
    }
  }
}
@inline(never)
func testObjectiveCBridgeStubFromNSDateRef() {
  let b = BridgeTester()
  for _ in 0 ..< 100_000 {
    let bridgedBegin = b.beginDate()
    let bridgedEnd = b.endDate()
    let _ = bridgedEnd.timeIntervalSince(bridgedBegin)
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubFromNSDateRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubFromNSDateRef()
    }
  }
}

@inline(never)
public func testObjectiveCBridgeStubToNSDateRef() {
  let b = BridgeTester()
  let d = NSDate()
  for _ in 0 ..< 100_000 {
    b.use(d)
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubToNSDateRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubToNSDateRef()
    }
  }
}


@inline(never)
func testObjectiveCBridgeStubNSDateRefAccess() {
  var remainders = 0.0
  let d = NSDate()
  for _ in 0 ..< 100_000 {
    remainders += d.timeIntervalSinceReferenceDate.truncatingRemainder(dividingBy: 10)
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubNSDateRefAccess(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDateRefAccess()
    }
  }
}

@inline(never)
func testObjectiveCBridgeStubNSDateMutationRef() {
  var d = NSDate()
  for _ in 0 ..< 100_000 {
      d = d.addingTimeInterval(1)
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubNSDateMutationRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDateMutationRef()
    }
  }
}

@inline(never)
func testObjectiveCBridgeStubURLAppendPathRef() {
  let startUrl = URL(string: "/")!
  for _ in 0 ..< 10_000 {
    var url = startUrl
    for _ in 0 ..< 10 {
      url = url.appendingPathComponent("foo")
    }
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubURLAppendPathRef(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubURLAppendPathRef()
    }
  }
}

@inline(never)
func testObjectiveCBridgeStubNSDataAppend() {
  let proto = NSMutableData()
  var value: UInt8 = 1
  for _ in 0 ..< 1_000 {
    let d = proto.mutableCopy() as! NSMutableData
    for _ in 0 ..< 100 {
       d.append(&value, length: 1)
    }
  }
}

@inline(never)
public func run_ObjectiveCBridgeStubNSDataAppend(N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDataAppend()
    }
  }
}
