// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 3 %s -o %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import StdlibUnittest
import StdlibUnittestFoundationExtras

let AppKitTests = TestSuite("AppKit_Swift3")

AppKitTests.test("NSEventMaskFromType") {
  let eventType: NSEventType = .keyDown
  let eventMask = NSEventMaskFromType(eventType)
  expectEqual(eventMask, .keyDown)
}

AppKitTests.test("NSLayoutPriority") {
	let highPriority: NSLayoutPriority = NSLayoutPriorityDefaultHigh

  let adjustedPriority1 = highPriority + 1
  let adjustedPriority1RawValue: Float = NSLayoutPriorityDefaultHigh + 1
  expectEqual(adjustedPriority1, adjustedPriority1RawValue)

  let adjustedPriority2 = highPriority - 5.0
  let adjustedPriority2RawValue: Float = NSLayoutPriorityDefaultHigh - 5.0
  expectEqual(adjustedPriority2, adjustedPriority2RawValue)

  let adjustedPriority3 = 5.0 + highPriority
  let adjustedPriority3RawValue: Float = 5.0 + NSLayoutPriorityDefaultHigh
  expectEqual(adjustedPriority3, adjustedPriority3RawValue)

	// Inferred typing from result type
  let adjustedPriority4: NSLayoutPriority = NSLayoutPriorityDefaultHigh + 2.0
  let adjustedPriority4RawValue: Float = NSLayoutPriorityDefaultHigh + 2.0
  expectEqual(adjustedPriority4, adjustedPriority4RawValue)

	// Comparable
	expectTrue(adjustedPriority1 > adjustedPriority2)
	expectTrue(adjustedPriority2 < adjustedPriority1)

	// Compound assignment
	var variablePriority: NSLayoutPriority = NSLayoutPriorityDefaultHigh
	variablePriority += 1
	variablePriority -= 5.0
	let variablePriorityRawValue: Float = NSLayoutPriorityDefaultHigh + 1 - 5.0
	expectEqual(variablePriority, variablePriorityRawValue)
}

runAllTests()
