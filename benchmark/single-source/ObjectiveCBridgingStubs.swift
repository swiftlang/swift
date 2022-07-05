//===--- ObjectiveCBridgingStubs.swift ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
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

let t: [BenchmarkCategory] = [.validation, .bridging]
let ts: [BenchmarkCategory] = [.validation, .String, .bridging]
let bs: [BenchmarkCategory] = [.String, .bridging]

public let benchmarks = [
  BenchmarkInfo(name: "ObjectiveCBridgeStubDataAppend",
    runFunction: run_ObjectiveCBridgeStubDataAppend, tags: t,
    legacyFactor: 20),
  BenchmarkInfo(name: "ObjectiveCBridgeStubDateAccess",
    runFunction: run_ObjectiveCBridgeStubDateAccess, tags: t),
  BenchmarkInfo(name: "ObjectiveCBridgeStubDateMutation",
    runFunction: run_ObjectiveCBridgeStubDateMutation, tags: t),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromArrayOfNSString2",
    runFunction: run_ObjectiveCBridgeStubFromArrayOfNSString, tags: t,
    legacyFactor: 10),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSDate",
    runFunction: run_ObjectiveCBridgeStubFromNSDate, tags: t,
    legacyFactor: 10),
  BenchmarkInfo(name: "ObjectiveCBridgeStubFromNSString",
    runFunction: run_ObjectiveCBridgeStubFromNSString, tags: t),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToArrayOfNSString2",
    runFunction: run_ObjectiveCBridgeStubToArrayOfNSString, tags: t,
    legacyFactor: 20),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSDate2",
    runFunction: run_ObjectiveCBridgeStubToNSDate, tags: t,
    legacyFactor: 10),
  BenchmarkInfo(name: "ObjectiveCBridgeStubToNSString",
    runFunction: run_ObjectiveCBridgeStubToNSString, tags: t,
    legacyFactor: 10),
  BenchmarkInfo(name: "ObjectiveCBridgeStubURLAppendPath2",
    runFunction: run_ObjectiveCBridgeStubURLAppendPath, tags: t,
    legacyFactor: 10),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqual",
    runFunction: run_ObjectiveCBridgeStringIsEqual, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqual2",
    runFunction: run_ObjectiveCBridgeStringIsEqual2, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringIsEqualAllSwift",
    runFunction: run_ObjectiveCBridgeStringIsEqualAllSwift, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringCompare",
    runFunction: run_ObjectiveCBridgeStringCompare, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringCompare2",
    runFunction: run_ObjectiveCBridgeStringCompare2, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringGetASCIIContents",
    runFunction: run_ObjectiveCBridgeStringGetASCIIContents, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringGetUTF8Contents",
    runFunction: run_ObjectiveCBridgeStringGetUTF8Contents, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringRangeOfString", //should be BridgeString.find.mixed
    runFunction: run_ObjectiveCBridgeStringRangeOfString, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "BridgeString.find.native",
    runFunction: run_ObjectiveCBridgeStringRangeOfStringAllSwift, tags: bs,
    setUpFunction: setup_SpecificRangeOfStringBridging),
  BenchmarkInfo(name: "BridgeString.find.native.nonASCII",
    runFunction: run_ObjectiveCBridgeStringRangeOfStringAllSwiftNonASCII, tags: bs,
    setUpFunction: setup_SpecificRangeOfStringBridging),
  BenchmarkInfo(name: "BridgeString.find.native.long",
    runFunction: run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystack, tags: bs,
    setUpFunction: setup_SpecificRangeOfStringBridging),
  BenchmarkInfo(name: "BridgeString.find.native.longBoth",
    runFunction: run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystackLongNeedle, tags: bs,
    setUpFunction: setup_SpecificRangeOfStringBridging),
  BenchmarkInfo(name: "BridgeString.find.native.longNonASCII",
    runFunction: run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystackNonASCII, tags: bs,
    setUpFunction: setup_SpecificRangeOfStringBridging),
  BenchmarkInfo(name: "ObjectiveCBridgeStringHash",
    runFunction: run_ObjectiveCBridgeStringHash, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringUTF8String",
    runFunction: run_ObjectiveCBridgeStringUTF8String, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
  BenchmarkInfo(name: "ObjectiveCBridgeStringCStringUsingEncoding",
    runFunction: run_ObjectiveCBridgeStringCStringUsingEncoding, tags: ts,
    setUpFunction: setup_StringBridgeBenchmark),
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
   check(str != "" && str == "Default string value no tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
   for _ in 0 ..< 1_000 {
     b.test(from: str)
   }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
   for _ in 0 ..< 100 {
     arr = b.testToArrayOfStrings()
     str = arr[0]
   }
   check(str != "" && str == "Default string value no tagged pointer")
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromArrayOfNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
   for _ in 0 ..< 50 {
     b.test(fromArrayOf: arr)
   }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubToArrayOfNSString(_ n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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

  for _ in 0 ..< 10_000 {
    let bridgedBegin = b.beginDate()
    let bridgedEnd = b.endDate()
    let _ = bridgedEnd.timeIntervalSince(bridgedBegin)
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubFromNSDate(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
public func run_ObjectiveCBridgeStubToNSDate(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
public func run_ObjectiveCBridgeStubDateAccess(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
public func run_ObjectiveCBridgeStubDateMutation(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  for _ in 0 ..< 10 {
    var url = startUrl
    for _ in 0 ..< 10 {
      url.appendPathComponent("foo")
    }
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubURLAppendPath(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
  for _ in 0 ..< 50 {
    var d = proto
    for _ in 0 ..< 100 {
       d.append(&value, count: 1)
    }
  }
}
#endif

@inline(never)
public func run_ObjectiveCBridgeStubDataAppend(n: Int) {
#if _runtime(_ObjC)
  for _ in 0 ..< n {
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
public func run_ObjectiveCBridgeStringIsEqual(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testIsEqualToString()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringIsEqual2(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testIsEqualToString2()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringIsEqualAllSwift(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testIsEqualToStringAllSwift()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringCompare(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testCompare()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringCompare2(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testCompare2()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringGetASCIIContents(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testGetASCIIContents()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringGetUTF8Contents(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testGetUTF8Contents()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfString(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testRangeOfString()
    }
  }
  #endif
}

@inline(__always)
func run_rangeOfStringSpecific(needle: String, haystack: String, n: Int) {
#if _runtime(_ObjC)
  b.testRangeOfStringSpecific(withNeedle: needle, haystack: haystack, n: n)
#endif
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfStringAllSwift(n: Int) {
  run_rangeOfStringSpecific(needle: "y", haystack: "The quick brown fox jumps over the lazy dog", n: 100 * n)
}

var longNativeASCII: String! = nil
var longNativeNonASCII: String! = nil
public func setup_SpecificRangeOfStringBridging() {
  setup_StringBridgeBenchmark()
  longNativeASCII = Array(repeating: "The quick brown fox jump over the lazy dog", count: 1000).joined() + "s"
  longNativeNonASCII = "ü" + longNativeASCII + "ö"
  
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystack(n: Int) {
  run_rangeOfStringSpecific(needle: "s", haystack: longNativeASCII, n: n)
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystackNonASCII(n: Int) {
  run_rangeOfStringSpecific(needle: "s", haystack: longNativeNonASCII, n: n)
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfStringAllSwiftNonASCII(n: Int) {
  run_rangeOfStringSpecific(needle: "ü", haystack: "The quick brown fox jump over the lazy dogü", n: 100 * n)
}

@inline(never)
public func run_ObjectiveCBridgeStringRangeOfStringAllSwiftLongHaystackLongNeedle(n: Int) {
  run_rangeOfStringSpecific(needle: "The quick brown fox jump over the lazy dogs", haystack: longNativeASCII, n: n)
}

@inline(never)
public func run_ObjectiveCBridgeStringHash(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testHash()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringUTF8String(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testUTF8String()
    }
  }
  #endif
}

@inline(never)
public func run_ObjectiveCBridgeStringCStringUsingEncoding(n: Int) {
  #if _runtime(_ObjC)
  for _ in 0 ..< n {
    autoreleasepool {
      b.testCStringUsingEncoding()
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
