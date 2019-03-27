// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: libdispatch
// REQUIRES: foundation

import Dispatch
import Foundation
import StdlibUnittest

var DispatchAPI = TestSuite("DispatchAPI")

DispatchAPI.test("DispatchTime.addSubtractDateConstants") {
	var then = DispatchTime.now() + Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: UInt64.max), then)

	then = DispatchTime.now() + Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: UInt64.max), then)
}

DispatchAPI.test("DispatchWallTime.addSubtractDateConstants") {
	let distantPastRawValue = DispatchWallTime.distantFuture.rawValue - UInt64(1)

	var then = DispatchWallTime.now() + Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() + Date.distantPast.timeIntervalSinceNow
	expectEqual(distantPastRawValue, then.rawValue)

	then = DispatchWallTime.now() - Date.distantFuture.timeIntervalSinceNow
	expectEqual(distantPastRawValue, then.rawValue)

	then = DispatchWallTime.now() - Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchWallTime.distantFuture, then)
}

runAllTests()
