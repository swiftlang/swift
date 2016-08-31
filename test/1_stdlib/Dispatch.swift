// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Dispatch
import Foundation
import StdlibUnittest


defer { runAllTests() }

var DispatchAPI = TestSuite("DispatchAPI")

DispatchAPI.test("constants") {
  expectEqual(2147483648, DispatchSource.ProcessEvent.exit.rawValue)
  expectEqual(0, DispatchData.empty.endIndex)

  // This is a lousy test, but really we just care that
  // DISPATCH_QUEUE_CONCURRENT comes through at all.
  _ = DispatchQueue.Attributes.concurrent
}

DispatchAPI.test("OS_OBJECT support") {
  let mainQueue = DispatchQueue.main as AnyObject
  expectTrue(mainQueue is DispatchQueue)

  // This should not be optimized out, and should succeed.
  expectNotEmpty(mainQueue as? DispatchQueue)
}

DispatchAPI.test("DispatchGroup creation") {
  let group = DispatchGroup()
  expectNotEmpty(group)
}

DispatchAPI.test("dispatch_block_t conversions") {
  var counter = 0
  let closure = { () -> Void in
    counter += 1
  }

  typealias Block = @convention(block) () -> ()
  let block = closure as Block
  block()
  expectEqual(1, counter)

  let closureAgain = block as () -> Void
  closureAgain()
  expectEqual(2, counter)
}

if #available(OSX 10.10, iOS 8.0, *) {
  DispatchAPI.test("dispatch_block_t identity") {
    let block = DispatchWorkItem(flags: .inheritQoS) {
      _ = 1
    }

    DispatchQueue.main.async(execute: block)
    // This will trap if the block's pointer identity is not preserved.
    block.cancel()
  }
}

DispatchAPI.test("dispatch_data_t enumeration") {
	// Ensure we can iterate the empty iterator
	for x in DispatchData.empty {
		_ = 1
	}
}

DispatchAPI.test("dispatch_data_t deallocator") {
	let q = DispatchQueue(label: "dealloc queue")
	var t = 0

	autoreleasepool {
		let size = 1024
		let p = UnsafeMutablePointer<UInt8>.allocate(capacity: size)
		let d = DispatchData(bytesNoCopy: UnsafeBufferPointer(start: p, count: size), deallocator: .custom(q, {
			t = 1
		}))
	}

	q.sync {
		expectEqual(1, t)
	}
}
