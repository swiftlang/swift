// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 %s -o %t/a.out-4 && %target-codesign %t/a.out-4 && %target-run %t/a.out-4
// RUN: %target-build-swift -swift-version 4.2 %s -o %t/a.out-4.2 && %target-codesign %t/a.out-4.2 && %target-run %t/a.out-4.2
// REQUIRES: executable_test
// REQUIRES: dispatch

import Dispatch
#if os(macOS) || os(iOS)
import Foundation
#endif
import StdlibUnittest

defer { runAllTests() }

var DispatchAPI = TestSuite("DispatchAPI")

DispatchAPI.test("dispatch_data_t enumeration") {
	// Ensure we can iterate the empty iterator
	for _ in DispatchData.empty {
		_ = 1
	}
}

DispatchAPI.test("dispatch_data_t deallocator") {
	let q = DispatchQueue(label: "dealloc queue")
	var t = 0

#if os(macOS) || os(iOS)
	let runWithPool: (()->())->() = { autoreleasepool { $0() } }
#else
	let runWithPool: (()->())->() = { $0() }
#endif

	runWithPool {
		let size = 1024
		let p = UnsafeMutablePointer<UInt8>.allocate(capacity: size)
		let _ = DispatchData(bytesNoCopy: UnsafeBufferPointer(start: p, count: size), deallocator: .custom(q, {
			t = 1
		}))
	}

	q.sync {
		expectEqual(1, t)
	}
}

DispatchAPI.test("DispatchData.copyBytes") {
	let source1: [UInt8] = [0, 1, 2, 3]
	let srcPtr1 = UnsafeBufferPointer(start: source1, count: source1.count)

	var dest: [UInt8] = [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
	let destPtr = UnsafeMutableBufferPointer(start: UnsafeMutablePointer(&dest),
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

	ptr = UnsafeBufferPointer<UInt8>(start: nil, count: 0)
	data = DispatchData(bytes: ptr)
	expectEqual(data.count, 0)

	data = DispatchData(bytesNoCopy: ptr, deallocator: .custom(nil, {}))
	expectEqual(data.count, 0)
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

DispatchAPI.test("DispatchData.enumerateBytes") {
	let count = 240
	var bytes = [UInt8]()
	for i in 0..<count {
		bytes.append(UInt8(i))
	}
	let ptr = UnsafeRawBufferPointer(start: bytes, count: bytes.count)
	let data = DispatchData(bytes: ptr)

	var nextIndex = 0
#if swift(>=4.2)
	data.enumerateBytes({ ptr, offset, stop in
		for i in 0..<ptr.count {
			expectEqual(ptr[i + offset], UInt8(i + offset))
		}
		nextIndex = offset + ptr.count
	})
#else
	data.enumerateBytes(block: { ptr, offset, stop in
		for i in 0..<ptr.count {
			expectEqual(ptr[i + offset], UInt8(i + offset))
		}
		nextIndex = offset + ptr.count
	})
#endif
	expectEqual(nextIndex, data.count)
}

DispatchAPI.test("DispatchData.enumerateBytesTrailingClosure") {
	let count = 240
	var bytes = [UInt8]()
	for i in 0..<count {
		bytes.append(UInt8(i))
	}
	let ptr = UnsafeRawBufferPointer(start: bytes, count: bytes.count)
	let data = DispatchData(bytes: ptr)

	var nextIndex = 0
	data.enumerateBytes() { ptr, offset, stop in
		for i in 0..<ptr.count {
			expectEqual(ptr[i + offset], UInt8(i + offset))
		}
		nextIndex = offset + ptr.count
	}
	expectEqual(nextIndex, data.count)
}
