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

DispatchAPI.test("dispatch_block_t conversions") {
  var counter = 0
  let closure = { () -> Void in
    counter += 1
  }

  let block = closure as dispatch_block_t
  block()
  expectEqual(1, counter)

  let closureAgain = block as () -> Void
  closureAgain()
  expectEqual(2, counter)
}

if #available(OSX 10.10, iOS 8.0, *) {
  DispatchAPI.test("dispatch_block_t identity") {
    let block = dispatch_block_create(DISPATCH_BLOCK_INHERIT_QOS_CLASS) {
      _ = 1
    }

    dispatch_async(dispatch_get_main_queue(), block)
    // This will trap if the block's pointer identity is not preserved.
    dispatch_block_cancel(block)
  }

  DispatchAPI.test("dispatch_block_t conversions with dispatch_block_create") {
    var counter = 0

    let block = dispatch_block_create(dispatch_block_flags_t(0)) {
      counter += 1
    }
    block()
    expectEqual(1, counter)

    let closure = block as () -> Void
    closure()
    expectEqual(2, counter)

    let blockAgain = closure as dispatch_block_t
    blockAgain()
    expectEqual(3, counter)
  }
}
