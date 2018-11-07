//===--- ObjectiveCBridgingStubs.swift ------------------------------------===//
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
#if _runtime(_ObjC)
import ObjectiveCTests
#endif

public let ObjectiveCBridgingStubs = [
  BenchmarkInfo(name: "ObjectiveCBridgeStubDataAppend", runFunction: run_ObjectiveCBridgeStubDataAppend, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubDateAccess", runFunction: run_ObjectiveCBridgeStubDateAccess, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubDateMutation", runFunction: run_ObjectiveCBridgeStubDateMutation, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromArrayOfNSString2", runFunction: run_ObjectiveCBridgeStubFromArrayOfNSString, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSDate", runFunction: run_ObjectiveCBridgeStubFromNSDate, tags: [.validation, .bridging, .unstable]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSString", runFunction: run_ObjectiveCBridgeStubFromNSString, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToArrayOfNSString2", runFunction: run_ObjectiveCBridgeStubToArrayOfNSString, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSDate2", runFunction: run_ObjectiveCBridgeStubToNSDate, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSString", runFunction: run_ObjectiveCBridgeStubToNSString, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStubURLAppendPath2", runFunction: run_ObjectiveCBridgeStubURLAppendPath, tags: [.validation, .bridging]),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqual", runFunction: run_ObjectiveCBridgeStringIsEqual, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqual2", runFunction: run_ObjectiveCBridgeStringIsEqual2, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqualAllSwift", runFunction: run_ObjectiveCBridgeStringIsEqualAllSwift, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringCompare", runFunction: run_ObjectiveCBridgeStringCompare, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringCompare2", runFunction: run_ObjectiveCBridgeStringCompare2, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringGetASCIIContents", runFunction: run_ObjectiveCBridgeStringGetASCIIContents, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringGetUTF8Contents", runFunction: run_ObjectiveCBridgeStringGetUTF8Contents, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringRangeOfString", runFunction: run_ObjectiveCBridgeStringRangeOfString, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringHash", runFunction: run_ObjectiveCBridgeStringHash, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringUTF8String", runFunction: run_ObjectiveCBridgeStringUTF8String, tags: [.validation, .String, .bridging], setUpFunction: setup_StringBridgeBenchmark),
]

var b:BridgeTester! = nil

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromNSString() {
   let b = BridgeTester()
   var str = ""
   for _ in 0 ..< 10_000 {
     str = b.testToString()
   }
   CheckResults(str != "" && str == "Default string value no tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubFromNSString()
    }
  }
#endif
}


#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubToNSString() {
   let b = BridgeTester()
   let str = "hello world"
   for _ in 0 ..< 10_000 {
     b.test(from: str)
   }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubToNSString()
    }
  }
#endif
}
#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromArrayOfNSString() {
   let b = BridgeTester()
   var arr : [String] = []
   var str = ""
   for _ in 0 ..< 1_000 {
     arr = b.testToArrayOfStrings()
     str = arr[0]
   }
   CheckResults(str != "" && str == "Default string value no tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromArrayOfNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubFromArrayOfNSString()
    }
  }
#endif
}
#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubToArrayOfNSString() {
   let b = BridgeTester()
   let str = "hello world"
   let arr = [str, str, str, str, str, str, str, str, str, str]
   for _ in 0 ..< 1_000 {
     b.test(fromArrayOf: arr)
   }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToArrayOfNSString(_ N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubToArrayOfNSString()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubFromNSDate() {
  let b = BridgeTester()

  for _ in 0 ..< 100_000 {
    let bridgedBegin = b.beginDate()
    let bridgedEnd = b.endDate()
    let _ = bridgedEnd.timeIntervalSince(bridgedBegin)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSDate(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubFromNSDate()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
public func testObjectiveCBridgeStubToNSDate() {
  let b = BridgeTester()
  let d = Date()
  for _ in 0 ..< 1_000 {
    b.use(d)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSDate(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< 10 * N {
    autoreleasepool {
      testObjectiveCBridgeStubToNSDate()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubDateAccess() {
  var remainders = 0.0
  let d = Date()
  for _ in 0 ..< 100_000 {
    remainders += d.timeIntervalSinceReferenceDate.truncatingRemainder(dividingBy: 10)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubDateAccess(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubDateAccess()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubDateMutation() {
  var d = Date()
  for _ in 0 ..< 100_000 {
      d += 1
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubDateMutation(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubDateMutation()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubURLAppendPath() {
  let startUrl = URL(string: "/")!
  for _ in 0 ..< 100 {
    var url = startUrl
    for _ in 0 ..< 10 {
      url.appendPathComponent("foo")
    }
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubURLAppendPath(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubURLAppendPath()
    }
  }
#endif
}

#if _runtime(_ObjC)
@inline(never)
func testObjectiveCBridgeStubDataAppend() {
  let proto = Data()
  var value: UInt8 = 1
  for _ in 0 ..< 1_000 {
    var d = proto
    for _ in 0 ..< 100 {
       d.append(&value, count: 1)
    }
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubDataAppend(N: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      testObjectiveCBridgeStubDataAppend()
    }
  }
#endif
}

@inline(never)
internal func getStringsToBridge() -> [String] {
  let strings1 = ["hello", "the quick brown fox jumps over the lazy dog", "the quick brown fox jumps over the lazy dög"]
  return strings1 + strings1.map { $0 + $0 } //mix of literals and non-literals
}

@inline(never)
public func run_ObjectiveCBridgeStringIsEqual(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testIsEqualToString()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringIsEqual2(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testIsEqualToString2()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringIsEqualAllSwift(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testIsEqualToStringAllSwift()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringCompare(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testCompare()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringCompare2(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testCompare2()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringGetASCIIContents(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testGetASCIIContents()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringGetUTF8Contents(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testGetUTF8Contents()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfString(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testRangeOfString()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringHash(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testHash()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringUTF8String(N: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< N {
    autoreleasepool {
      b.testUTF8String()
    }
  }
  #endif
}

@inline(never)
public func setup_StringBridgeBenchmark() {
#if _runtime(_ObjC)
  b = BridgeTester()
  b.setUpStringTests(getStringsToBridge())
#endif
}
