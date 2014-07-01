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

nsValueBridging.test("NSPoint") {
  let nsValue = bridgeToObjectiveC(NSPoint(x: 5, y: 7)) as NSValue
  let swiftValue: NSPoint = bridgeFromObjectiveC(nsValue, NSPoint.self)
  expectEqual(swiftValue.x, 5)
  expectEqual(swiftValue.y, 7)
}

nsValueBridging.test("NSSize") {
  let nsValue = bridgeToObjectiveC(NSSize(width: 5.5, height: 7.5)) as NSValue
  let swiftValue: NSSize = bridgeFromObjectiveC(nsValue, NSSize.self)
  expectEqual(swiftValue.width, 5.5)
  expectEqual(swiftValue.height, 7.5)
}

nsValueBridging.test("NSRect") {
  let nsValue = bridgeToObjectiveC(
    NSRect(x: 11, y: 13, width: 5.5, height: 7.5)) as NSValue
  let swiftValue: NSRect = bridgeFromObjectiveC(nsValue, NSRect.self)
  expectEqual(swiftValue.origin.x, 11)
  expectEqual(swiftValue.origin.y, 13)
  expectEqual(swiftValue.size.width, 5.5)
  expectEqual(swiftValue.size.height, 7.5)
}

nsValueBridging.test("NSRange") {
  let nsValue = bridgeToObjectiveC(NSRange(location: 17, length: 19)) as NSValue
  let swiftValue: NSRange = bridgeFromObjectiveC(nsValue, NSRange.self)
  expectEqual(swiftValue.location, 17)
  expectEqual(swiftValue.length, 19)
}

nsValueBridging.run()

// CHECK: {{^}}NSValueBridging: All tests passed
