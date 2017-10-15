// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test
// REQUIRES: OS=macosx

import StdlibUnittest
import Swift
import SwiftPrivate

import Darwin

var str = "Lorem ipsum dolor sit amet\r\n 🌎 🇺🇸🇨🇦🍁"

func checkString(_unused: UnsafeMutableRawPointer) -> UnsafeMutableRawPointer? {
	expectEqual(str.count, str.reversed().count)
	return nil
}

var PThreadTests = TestSuite("Thread Local Storage")

// Test that the destructor for our TLS is invoked
PThreadTests.test("destructor") {
#if INTERNAL_CHECKS_ENABLED
	let numIters = 100
	for _ in 0..<numIters {
		var thread : pthread_t? = nil
		guard pthread_create(&thread, nil, checkString, &thread) == 0 else {
			fatalError("pthread_create failed")
		}
		guard pthread_join(thread!, nil) == 0 else {
			fatalError("pthread_join failed")
		}
	}
	expectEqual(numIters, _destroyTLSCounter.load())
#endif
	var x = 1
	checkString(_unused: &x)
}

runAllTests()

