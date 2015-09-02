// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Dispatch
import Foundation
import StdlibUnittest

defer { runAllTests() }

var DispatchAPI = TestSuite("DispatchAPI")

DispatchAPI.test("constants") {
  expectEqual(2147483648, DISPATCH_PROC_EXIT)
  expectEqual("<>", dispatch_data_empty.description)

  // This is a lousy test, but really we just care that
  // DISPATCH_QUEUE_CONCURRENT comes through at all.
  _ = DISPATCH_QUEUE_CONCURRENT as AnyObject
  _ = DISPATCH_QUEUE_CONCURRENT.description
}

DispatchAPI.test("OS_OBJECT support") {
  let mainQueue = dispatch_get_main_queue() as AnyObject
  expectTrue(mainQueue is dispatch_queue_t)

  // This should not be optimized out, and should succeed.
  expectNotEmpty(mainQueue as? dispatch_queue_t)
}

