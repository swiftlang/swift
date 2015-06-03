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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// XFAIL: interpret
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation

var nsValueBridging = TestSuite("NSValueBridging")

nsValueBridging.test("NSRange") {
  let nsValue = _bridgeToObjectiveC(NSRange(location: 17, length: 19)) as! NSValue
  let swiftValue: NSRange = _forceBridgeFromObjectiveC(nsValue, NSRange.self)
  expectEqual(17, swiftValue.location)
  expectEqual(19, swiftValue.length)
}

runAllTests()

