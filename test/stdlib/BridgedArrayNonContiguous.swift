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

suite.test("Bridged NSArray without direct memory sharing") {

  var arr = (0..<100).map({ _ in NSObject() as AnyObject})
  let identifiers = arr.map(ObjectIdentifier.init)

  let nsArray = arr.withUnsafeBufferPointer {
    NSArray(objects: $0.baseAddress, count: $0.count)
  }
  arr = []

  for i in 0..<nsArray.count {
    let bridged = nsArray as! [NSObject]
    let x: NSObject? = bridged.withContiguousStorageIfAvailable { $0[i] }

    expectNotNil(x)
    if let x {
      expectEqual(identifiers[i], ObjectIdentifier(x))
    }
  }
}

suite.test("Bridged NSArray as Span")
.skip(.custom(
  { if #available(SwiftStdlib 6.2, *) { false } else { true } },
  reason: "Requires Swift 6.2's standard library"
))
.code {
  guard #available(SwiftStdlib 6.2, *) else { return }
  
  var arr = (0..<100).map({ _ in NSObject() as AnyObject})
  let identifiers = arr.map(ObjectIdentifier.init)

  let nsArray = arr.withUnsafeBufferPointer {
    NSArray(objects: $0.baseAddress, count: $0.count)
  }
  arr = []

  for i in 0..<nsArray.count {
    let bridged = nsArray as! [NSObject]
    let span = bridged.span

    let x: NSObject = span[i]
    expectEqual(identifiers[i], ObjectIdentifier(x))
  }  
}
