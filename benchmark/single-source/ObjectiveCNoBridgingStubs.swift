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
#if _runtime(_ObjC)
import ObjectiveCTests
#endif

public let ObjectiveCNoBridgingStubs = [
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSStringRef", runFunction: run_ObjectiveCBridgeStubToNSStringRef, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSDateRef", runFunction: run_ObjectiveCBridgeStubToNSDateRef, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubNSDateRefAccess", runFunction: run_ObjectiveCBridgeStubNSDateRefAccess, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubNSDateMutationRef", runFunction: run_ObjectiveCBridgeStubNSDateMutationRef, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubNSDataAppend", runFunction: run_ObjectiveCBridgeStubNSDataAppend, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSStringRef", runFunction: run_ObjectiveCBridgeStubFromNSStringRef, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSDateRef", runFunction: run_ObjectiveCBridgeStubFromNSDateRef, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubURLAppendPathRef2", runFunction: run_ObjectiveCBridgeStubURLAppendPathRef, tags: [.validation, .bridging]),
]

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromNSStringRef() {
  let b = BridgeTester()
  var nsString : NSString = NSString()
  for _ in 0 ..< 10_000 {
    nsString = b.testToString()
  }
  CheckResults(nsString.isEqual(to: "Default string value no tagged pointer" as NSString))
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSStringRef(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubFromNSStringRef()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubToNSStringRef() {
   let b = BridgeTester()
   let str = NSString(cString: "hello world", encoding: String.Encoding.utf8.rawValue)!
   for _ in 0 ..< 10_000 {
     b.test(from: str)
   }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSStringRef(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubToNSStringRef()
    }
  }
#endif
}
#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromNSDateRef() {
  let b = BridgeTester()
  for _ in 0 ..< 100_000 {
    let bridgedBegin = b.beginDate()
    let bridgedEnd = b.endDate()
    let _ = bridgedEnd.timeIntervalSince(bridgedBegin)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSDateRef(N: Int) {
#if _runtime(_ObjC)
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubFromNSDateRef()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
public func testObjectiveCBridgeStubToNSDateRef() {
  let b = BridgeTester()
  let d = NSDate()
  for _ in 0 ..< 1_000 {
    b.use(d)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSDateRef(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< 100 * N {
    autoreleasepool {
      testObjectiveCBridgeStubToNSDateRef()
    }
  }
#endif
}


#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubNSDateRefAccess() {
  var remainders = 0.0
  let d = NSDate()
  for _ in 0 ..< 100_000 {
    remainders += d.timeIntervalSinceReferenceDate.truncatingRemainder(dividingBy: 10)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubNSDateRefAccess(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubNSDateRefAccess()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubNSDateMutationRef() {
  var d = NSDate()
  for _ in 0 ..< 100 {
      d = d.addingTimeInterval(1)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubNSDateMutationRef(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< 100 * N {
    autoreleasepool {
      testObjectiveCBridgeStubNSDateMutationRef()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubURLAppendPathRef() {
  let startUrl = URL(string: "/")!
  for _ in 0 ..< 100 {
    var url = startUrl
    for _ in 0 ..< 10 {
      url = url.appendingPathComponent("foo")
    }
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubURLAppendPathRef(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
   autoreleasepool {
     testObjectiveCBridgeStubURLAppendPathRef()
   }
  }
#endif
}

#if _runtime(_ObjC)
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
#endif

@inline(never)
public func run_ObjectiveCBridgeStubNSDataAppend(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubNSDataAppend()
    }
  }
#endif
}
