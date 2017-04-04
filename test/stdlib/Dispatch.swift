// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out_swift3 -swift-version 3
// RUN: %target-build-swift %s -o %t/a.out_swift4 -swift-version 4
//
// RUN: %target-run %t/a.out_swift3
// RUN: %target-run %t/a.out_swift4
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
  expectNotNil(mainQueue as? DispatchQueue)
}

DispatchAPI.test("DispatchGroup creation") {
  let group = DispatchGroup()
  expectNotNil(group)
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
		let d = DispatchData(bytesNoCopy: UnsafeRawBufferPointer(start: p, count: size), deallocator: .custom(q, {
			t = 1
		}))
	}

	q.sync {
		expectEqual(1, t)
	}
}

DispatchAPI.test("DispatchTime comparisons") {
    do {
        let now = DispatchTime.now()
        checkComparable([now, now + .milliseconds(1), .distantFuture], oracle: {
            return $0 < $1 ? .lt : $0 == $1 ? .eq : .gt
        })
    }

    do {
        let now = DispatchWallTime.now()
        checkComparable([now, now + .milliseconds(1), .distantFuture], oracle: {
            return $0 < $1 ? .lt : $0 == $1 ? .eq : .gt
        })
    }
}

DispatchAPI.test("DispatchTime.addSubtract") {
	var then = DispatchTime.now() + Double.infinity
	expectEqual(DispatchTime.distantFuture, then)

	then = DispatchTime.now() + Double.nan
	expectEqual(DispatchTime.distantFuture, then)

	then = DispatchTime.now() - Double.infinity
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Double.nan
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)
}

DispatchAPI.test("DispatchWallTime.addSubtract") {
	var then = DispatchWallTime.now() + Double.infinity
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() + Double.nan
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() - Double.infinity
	expectEqual(DispatchWallTime.distantFuture.rawValue - UInt64(1), then.rawValue)

	then = DispatchWallTime.now() - Double.nan
	expectEqual(DispatchWallTime.distantFuture.rawValue - UInt64(1), then.rawValue)
}

DispatchAPI.test("DispatchTime.uptimeNanos") {
	let seconds = 1
	let nowMach = DispatchTime.now()
	let oneSecondFromNowMach = nowMach + .seconds(seconds)
	let nowNanos = nowMach.uptimeNanoseconds
	let oneSecondFromNowNanos = oneSecondFromNowMach.uptimeNanoseconds 
	let diffNanos = oneSecondFromNowNanos - nowNanos
	expectEqual(NSEC_PER_SEC, diffNanos)
}

DispatchAPI.test("DispatchData.copyBytes") {
	let source1: [UInt8] = [0, 1, 2, 3]
	let srcPtr1 = UnsafeBufferPointer(start: source1, count: source1.count)

	var dest: [UInt8] = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
	var destPtr = UnsafeMutableBufferPointer(start: UnsafeMutablePointer(&dest),
			count: dest.count)

	var dispatchData = DispatchData(bytes: srcPtr1)

	// Copy from offset 0
	var count = dispatchData.copyBytes(to: destPtr, from: 0..<2)
	expectEqual(count, 2)
	expectEqual(destPtr[0], 0)
	expectEqual(destPtr[1], 1)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 2
	count = dispatchData.copyBytes(to: destPtr, from: 2..<4)
	expectEqual(count, 2)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 3)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Add two more regions
	let source2: [UInt8] = [0x10, 0x11, 0x12, 0x13]
	let srcPtr2 = UnsafeBufferPointer(start: source2, count: source2.count)
	dispatchData.append(DispatchData(bytes: srcPtr2))

	let source3: [UInt8] = [0x14, 0x15, 0x16]
	let srcPtr3 = UnsafeBufferPointer(start: source3, count: source3.count)
	dispatchData.append(DispatchData(bytes: srcPtr3))

	// Copy from offset 0. Copies across the first two regions
	count = dispatchData.copyBytes(to: destPtr, from: 0..<6)
	expectEqual(count, 6)
	expectEqual(destPtr[0], 0)
	expectEqual(destPtr[1], 1)
	expectEqual(destPtr[2], 2)
	expectEqual(destPtr[3], 3)
	expectEqual(destPtr[4], 0x10)
	expectEqual(destPtr[5], 0x11)

	// Copy from offset 2. Copies across the first two regions
	count = dispatchData.copyBytes(to: destPtr, from: 2..<8)
	expectEqual(count, 6)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 3)
	expectEqual(destPtr[2], 0x10)
	expectEqual(destPtr[3], 0x11)
	expectEqual(destPtr[4], 0x12)
	expectEqual(destPtr[5], 0x13)

	// Copy from offset 3. Copies across all three regions
	count = dispatchData.copyBytes(to: destPtr, from: 3..<9)
	expectEqual(count, 6)
	expectEqual(destPtr[0], 3)
	expectEqual(destPtr[1], 0x10)
	expectEqual(destPtr[2], 0x11)
	expectEqual(destPtr[3], 0x12)
	expectEqual(destPtr[4], 0x13)
	expectEqual(destPtr[5], 0x14)

	// Copy from offset 5. Skips the first region and the first byte of the second
	count = dispatchData.copyBytes(to: destPtr, from: 5..<11)
	expectEqual(count, 6)
	expectEqual(destPtr[0], 0x11)
	expectEqual(destPtr[1], 0x12)
	expectEqual(destPtr[2], 0x13)
	expectEqual(destPtr[3], 0x14)
	expectEqual(destPtr[4], 0x15)
	expectEqual(destPtr[5], 0x16)

	// Copy from offset 8. Skips the first two regions
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	count = dispatchData.copyBytes(to: destPtr, from: 8..<11)
	expectEqual(count, 3)
	expectEqual(destPtr[0], 0x14)
	expectEqual(destPtr[1], 0x15)
	expectEqual(destPtr[2], 0x16)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 9. Skips the first two regions and the first byte of the third
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	count = dispatchData.copyBytes(to: destPtr, from: 9..<11)
	expectEqual(count, 2)
	expectEqual(destPtr[0], 0x15)
	expectEqual(destPtr[1], 0x16)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 9, but only 1 byte. Ends before the end of the data
	destPtr[1] = 0xFF
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	count = dispatchData.copyBytes(to: destPtr, from: 9..<10)
	expectEqual(count, 1)
	expectEqual(destPtr[0], 0x15)
	expectEqual(destPtr[1], 0xFF)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 2, but only 1 byte. This copy is bounded within the 
	// first region.
	destPtr[1] = 0xFF
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	count = dispatchData.copyBytes(to: destPtr, from: 2..<3)
	expectEqual(count, 1)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 0xFF)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)
}

DispatchAPI.test("DispatchData.copyBytesUnsafeRawBufferPointer") {
	let source1: [UInt8] = [0, 1, 2, 3]
	let srcPtr1 = UnsafeRawBufferPointer(start: source1, count: source1.count)

	var dest: [UInt8] = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
	let destPtr = UnsafeMutableRawBufferPointer(start: UnsafeMutablePointer(&dest),
			count: dest.count)
	var dispatchData = DispatchData(bytes: srcPtr1)

	// Copy from offset 0
	dispatchData.copyBytes(to: destPtr, from: 0..<2)
	expectEqual(destPtr[0], 0)
	expectEqual(destPtr[1], 1)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 2
	dispatchData.copyBytes(to: destPtr, from: 2..<4)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 3)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Add two more regions
	let source2: [UInt8] = [0x10, 0x11, 0x12, 0x13]
	let srcPtr2 = UnsafeRawBufferPointer(start: source2, count: source2.count)
	dispatchData.append(DispatchData(bytes: srcPtr2))

	let source3: [UInt8] = [0x14, 0x15, 0x16]
	let srcPtr3 = UnsafeRawBufferPointer(start: source3, count: source3.count)
	dispatchData.append(DispatchData(bytes: srcPtr3))

	// Copy from offset 0. Copies across the first two regions
	dispatchData.copyBytes(to: destPtr, from: 0..<6)
	expectEqual(destPtr[0], 0)
	expectEqual(destPtr[1], 1)
	expectEqual(destPtr[2], 2)
	expectEqual(destPtr[3], 3)
	expectEqual(destPtr[4], 0x10)
	expectEqual(destPtr[5], 0x11)

	// Copy from offset 2. Copies across the first two regions
	dispatchData.copyBytes(to: destPtr, from: 2..<8)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 3)
	expectEqual(destPtr[2], 0x10)
	expectEqual(destPtr[3], 0x11)
	expectEqual(destPtr[4], 0x12)
	expectEqual(destPtr[5], 0x13)

	// Copy from offset 3. Copies across all three regions
	dispatchData.copyBytes(to: destPtr, from: 3..<9)
	expectEqual(destPtr[0], 3)
	expectEqual(destPtr[1], 0x10)
	expectEqual(destPtr[2], 0x11)
	expectEqual(destPtr[3], 0x12)
	expectEqual(destPtr[4], 0x13)
	expectEqual(destPtr[5], 0x14)

	// Copy from offset 5. Skips the first region and the first byte of the second
	dispatchData.copyBytes(to: destPtr, from: 5..<11)
	expectEqual(destPtr[0], 0x11)
	expectEqual(destPtr[1], 0x12)
	expectEqual(destPtr[2], 0x13)
	expectEqual(destPtr[3], 0x14)
	expectEqual(destPtr[4], 0x15)
	expectEqual(destPtr[5], 0x16)

	// Copy from offset 8. Skips the first two regions
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	dispatchData.copyBytes(to: destPtr, from: 8..<11)
	expectEqual(destPtr[0], 0x14)
	expectEqual(destPtr[1], 0x15)
	expectEqual(destPtr[2], 0x16)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 9. Skips the first two regions and the first byte of the third
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	dispatchData.copyBytes(to: destPtr, from: 9..<11)
	expectEqual(destPtr[0], 0x15)
	expectEqual(destPtr[1], 0x16)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 9, but only 1 byte. Ends before the end of the data
	destPtr[1] = 0xFF
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	dispatchData.copyBytes(to: destPtr, from: 9..<10)
	expectEqual(destPtr[0], 0x15)
	expectEqual(destPtr[1], 0xFF)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)

	// Copy from offset 2, but only 1 byte. This copy is bounded within the 
	// first region.
	destPtr[1] = 0xFF
	destPtr[2] = 0xFF
	destPtr[3] = 0xFF
	destPtr[4] = 0xFF
	destPtr[5] = 0xFF
	dispatchData.copyBytes(to: destPtr, from: 2..<3)
	expectEqual(destPtr[0], 2)
	expectEqual(destPtr[1], 0xFF)
	expectEqual(destPtr[2], 0xFF)
	expectEqual(destPtr[3], 0xFF)
	expectEqual(destPtr[4], 0xFF)
	expectEqual(destPtr[5], 0xFF)
}

DispatchAPI.test("DispatchData.buffers") {
	let bytes = [UInt8(0), UInt8(1), UInt8(2), UInt8(2)]
	var ptr = UnsafeBufferPointer<UInt8>(start: bytes, count: bytes.count)
	var data = DispatchData(bytes: ptr)
	expectEqual(bytes.count, data.count)
	for i in 0..<data.count {
		expectEqual(data[i], bytes[i])
	}

	data = DispatchData(bytesNoCopy: ptr, deallocator: .custom(nil, {}))
	expectEqual(bytes.count, data.count)
	for i in 0..<data.count {
		expectEqual(data[i], bytes[i])
	}

	// Michael NOTE: disabled as part of buffer-pointer being non-nullable
	//
	// ptr = UnsafeBufferPointer<UInt8>(start: nil, count: 0)
	// data = DispatchData(bytes: ptr)
	// expectEqual(data.count, 0)

	// data = DispatchData(bytesNoCopy: ptr, deallocator: .custom(nil, {}))
	// expectEqual(data.count, 0)
}

DispatchAPI.test("DispatchData.bufferUnsafeRawBufferPointer") {
	let bytes = [UInt8(0), UInt8(1), UInt8(2), UInt8(2)]
	var ptr = UnsafeRawBufferPointer(start: bytes, count: bytes.count)
	var data = DispatchData(bytes: ptr)
	expectEqual(bytes.count, data.count)
	for i in 0..<data.count {
		expectEqual(data[i], bytes[i])
	}

	data = DispatchData(bytesNoCopy: ptr, deallocator: .custom(nil, {}))
	expectEqual(bytes.count, data.count)
	for i in 0..<data.count {
		expectEqual(data[i], bytes[i])
	}

	ptr = UnsafeRawBufferPointer(start: nil, count: 0)
	data = DispatchData(bytes: ptr)
	expectEqual(data.count, 0)

	data = DispatchData(bytesNoCopy: ptr, deallocator: .custom(nil, {}))
	expectEqual(data.count, 0)
}

DispatchAPI.test("DispatchIO.initRelativePath") {
	let q = DispatchQueue(label: "initRelativePath queue")
#if swift(>=4.0)
	let chan = DispatchIO(type: .random, path: "_REL_PATH_", oflag: O_RDONLY, mode: 0, queue: q, cleanupHandler: { (error) in })
	expectEqual(chan, nil)
#else
	expectCrashLater()
	let chan = DispatchIO(type: .random, path: "_REL_PATH_", oflag: O_RDONLY, mode: 0, queue: q, cleanupHandler: { (error) in })
	chan.setInterval(interval: .seconds(1)) // Dereference of unexpected nil should crash
#endif
}
