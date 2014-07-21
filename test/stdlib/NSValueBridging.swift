//===--- NSValueBridging.swift - Test bridging through NSValue ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest
import Foundation

var nsValueBridging = TestCase("NSValueBridging")

nsValueBridging.test("CGPoint") {
  let nsValue = _bridgeToObjectiveC(CGPoint(x: 5, y: 7)) as NSValue
  let swiftValue: CGPoint = _bridgeFromObjectiveC(nsValue, CGPoint.self)
  expectEqual(5, swiftValue.x)
  expectEqual(7, swiftValue.y)
}

nsValueBridging.test("CGSize") {
  let nsValue = _bridgeToObjectiveC(CGSize(width: 5.5, height: 7.5)) as NSValue
  let swiftValue: CGSize = _bridgeFromObjectiveC(nsValue, CGSize.self)
  expectEqual(5.5, swiftValue.width)
  expectEqual(7.5, swiftValue.height)
}

nsValueBridging.test("CGRect") {
  let nsValue = _bridgeToObjectiveC(
    CGRect(x: 11, y: 13, width: 5.5, height: 7.5)) as NSValue
  let swiftValue: CGRect = _bridgeFromObjectiveC(nsValue, CGRect.self)
  expectEqual(11, swiftValue.origin.x)
  expectEqual(13, swiftValue.origin.y)
  expectEqual(5.5, swiftValue.size.width)
  expectEqual(7.5, swiftValue.size.height)
}

nsValueBridging.test("NSRange") {
  let nsValue = _bridgeToObjectiveC(NSRange(location: 17, length: 19)) as NSValue
  let swiftValue: NSRange = _bridgeFromObjectiveC(nsValue, NSRange.self)
  expectEqual(17, swiftValue.location)
  expectEqual(19, swiftValue.length)
}

nsValueBridging.run()

// CHECK: {{^}}NSValueBridging: All tests passed
