//===--- ObjectiveCBridgingStubs.swift ------------------------------------===//
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

import TestsUtils
import Foundation
import ObjectiveCTests

@inline(never)
func testObjectiveCBridgeStubFromNSString() {
   let b = BridgeTester()
   var str = ""
   for _ in 0 ..< 10_000 {
     str = b.testToString()
   }
   CheckResults(str != "" && str == "Default string value no tagged pointer", "Wrong value returned")
}

@inline(never)
public func run_ObjectiveCBridgeStubFromNSString(_ N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubFromNSString()
    }
  }
}


@inline(never)
func testObjectiveCBridgeStubToNSString() {
   let b = BridgeTester()
   let str = "hello world"
   for _ in 0 ..< 10_000 {
     b.test(from: str)
   }
}

@inline(never)
public func run_ObjectiveCBridgeStubToNSString(_ N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubToNSString()
    }
  }
}
@inline(never)
func testObjectiveCBridgeStubFromArrayOfNSString() {
   let b = BridgeTester()
   var arr : [String] = []
   var str = ""
   for _ in 0 ..< 10_000 {
     arr = b.testToArrayOfStrings()
     str = arr[0]
   }
   CheckResults(str != "" && str == "Default string value no tagged pointer", "Wrong value returned")
}

@inline(never)
public func run_ObjectiveCBridgeStubFromArrayOfNSString(_ N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubFromArrayOfNSString()
    }
  }
}
@inline(never)
func testObjectiveCBridgeStubToArrayOfNSString() {
   let b = BridgeTester()
   let str = "hello world"
   let arr = [str, str, str, str, str, str, str, str, str, str]
   for _ in 0 ..< 10_000 {
     b.test(fromArrayOf: arr)
   }
}

@inline(never)
public func run_ObjectiveCBridgeStubToArrayOfNSString(_ N: Int) {
  autoreleasepool {
    for _ in 0 ..< N {
      testObjectiveCBridgeStubToArrayOfNSString()
    }
  }
}
