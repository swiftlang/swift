//===--- BridgedArrayNonContiguous.swift ----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift -enable-experimental-feature LifetimeDependence

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: swift_feature_LifetimeDependence

import StdlibUnittest

import Foundation

var suite = TestSuite("EagerLazyBridgingTests")
defer { runAllTests() }

var x: NSObject? = nil

suite.test("Bridged NSArray without direct memory sharing") {

  var arr = Array(repeating: NSObject(), count: 100) as [AnyObject]
  let nsArray:NSArray = NSArray(objects: &arr, count: 100)
  for _ in 0 ..< 5 {
    let tmp = nsArray as! [NSObject]
    x = tmp.withContiguousStorageIfAvailable {
      $0[0]
    }
    expectNotNil(x)
  }

  x = nil
}
