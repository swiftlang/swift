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

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromNSStringRef() {
  let b = BridgeTester()
  var nsString : NSString = NSString()
  for _ in 0 ..< 10_000 {
    nsString = b.testToString()
  }
  CheckResults(nsString.isEqual(to: "Default string value no tagged pointer" as NSString), "Wrong value returned")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSStringRef(N: Int) {
#if _runtime(_ObjC)
  autoreleasepool {
    for _ in 0 ..< N {
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
  autoreleasepool {
    for _ in 0 ..< N {
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
  for _ in 0 ..< 100_000 {
    b.use(d)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSDateRef(N: Int) {
#if _runtime(_ObjC)
  autoreleasepool {
    for _ in 0 ..< N {
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
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDateRefAccess()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubNSDateMutationRef() {
  var d = NSDate()
  for _ in 0 ..< 100_000 {
      d = d.addingTimeInterval(1)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubNSDateMutationRef(N: Int) {
#if _runtime(_ObjC)
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDateMutationRef()
    }
  }
#endif
}

#if _runtime(_ObjC)
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
#endif

@inline(never)
public func run_ObjectiveCBridgeStubURLAppendPathRef(N: Int) {
#if _runtime(_ObjC)
  autoreleasepool {
    for _ in 0 ..< N {
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
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubNSDataAppend()
    }
  }
#endif
}
