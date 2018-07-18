// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
//
// RUN: %target-run %t/a.out
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

DispatchAPI.test("Dispatch sync return value") {
  let value = 24
  let q = DispatchQueue(label: "Test")
  let result = q.sync() { return 24 }
  expectEqual(value, result)
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

DispatchAPI.test("DispatchTime.create") {
	var info = mach_timebase_info_data_t(numer: 1, denom: 1)
	mach_timebase_info(&info)
	let scales = info.numer != info.denom

	// Simple tests for non-overflow behavior
	var time = DispatchTime(uptimeNanoseconds: 0)
	expectEqual(time.uptimeNanoseconds, 0)

	time = DispatchTime(uptimeNanoseconds: 15 * NSEC_PER_SEC)
	expectEqual(time.uptimeNanoseconds, 15 * NSEC_PER_SEC)

	// On platforms where the timebase scale is not 1, the next two cases
	// overflow and become DISPATCH_TIME_FOREVER (UInt64.max) instead of trapping.
	time = DispatchTime(uptimeNanoseconds: UInt64.max - 1)
	expectEqual(time.uptimeNanoseconds, scales ? UInt64.max : UInt64.max - UInt64(1))

	time = DispatchTime(uptimeNanoseconds: UInt64.max / 2) 
	expectEqual(time.uptimeNanoseconds, scales ? UInt64.max : UInt64.max / 2)

	// UInt64.max must always be returned as UInt64.max.
	time = DispatchTime(uptimeNanoseconds: UInt64.max)
	expectEqual(time.uptimeNanoseconds, UInt64.max)
}

DispatchAPI.test("DispatchTime.addSubtract") {
	var then = DispatchTime.now() + Double.infinity
	expectEqual(DispatchTime.distantFuture, then)

	then = DispatchTime.now() + Double.nan
	expectEqual(DispatchTime.distantFuture, then)

	then = DispatchTime.now() - Double.infinity
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Double.nan
	expectEqual(DispatchTime.distantFuture, then)

	then = DispatchTime.now() + Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: UInt64.max), then)

	then = DispatchTime.now() + Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: 1), then)

	then = DispatchTime.now() - Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchTime(uptimeNanoseconds: UInt64.max), then)
}

DispatchAPI.test("DispatchWallTime.addSubtract") {
	let distantPastRawValue = DispatchWallTime.distantFuture.rawValue - UInt64(1)

	var then = DispatchWallTime.now() + Double.infinity
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() + Double.nan
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() - Double.infinity
	expectEqual(distantPastRawValue, then.rawValue)

	then = DispatchWallTime.now() - Double.nan
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() + Date.distantFuture.timeIntervalSinceNow
	expectEqual(DispatchWallTime.distantFuture, then)

	then = DispatchWallTime.now() + Date.distantPast.timeIntervalSinceNow
	expectEqual(distantPastRawValue, then.rawValue)

	then = DispatchWallTime.now() - Date.distantFuture.timeIntervalSinceNow
	expectEqual(distantPastRawValue, then.rawValue)

	then = DispatchWallTime.now() - Date.distantPast.timeIntervalSinceNow
	expectEqual(DispatchWallTime.distantFuture, then)
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

DispatchAPI.test("DispatchIO.initRelativePath") {
	let q = DispatchQueue(label: "initRelativePath queue")
	let chan = DispatchIO(type: .random, path: "_REL_PATH_", oflag: O_RDONLY, mode: 0, queue: q, cleanupHandler: { (error) in })
	expectEqual(chan, nil)
}

if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
	var block = DispatchWorkItem(qos: .unspecified, flags: .assignCurrentContext) {}
	DispatchAPI.test("DispatchSource.replace") {
		let g = DispatchGroup()
		let q = DispatchQueue(label: "q")
		let ds = DispatchSource.makeUserDataReplaceSource(queue: q)
		var lastValue = UInt(0)
		var nextValue = UInt(1)
		let maxValue = UInt(1 << 24)

		ds.setEventHandler() {
			let value = ds.data;
			expectTrue(value > lastValue)	 // Values must increase
			expectTrue((value & (value - 1)) == 0) // Must be power of two
			lastValue = value
			if value == maxValue {
				g.leave()
			}
		}
		ds.activate()

		g.enter()
		block = DispatchWorkItem(qos: .unspecified, flags: .assignCurrentContext) {
			ds.replace(data: nextValue)
			nextValue <<= 1
			if nextValue <= maxValue {
				q.asyncAfter(
					deadline: DispatchTime.now() + DispatchTimeInterval.milliseconds(1),
					execute: block)
			}
		}
		q.asyncAfter(
			deadline: DispatchTime.now() + DispatchTimeInterval.milliseconds(1),
			execute: block)

		let result = g.wait(timeout: DispatchTime.now() + .seconds(30))
		expectTrue(result == .success)
	}
}

DispatchAPI.test("DispatchTimeInterval") {
	// Basic tests that the correct value is stored and the == method works
	for i in stride(from:1, through: 100, by: 5) {
		expectEqual(DispatchTimeInterval.seconds(i), DispatchTimeInterval.milliseconds(i * 1000))
		expectEqual(DispatchTimeInterval.milliseconds(i), DispatchTimeInterval.microseconds(i * 1000))
		expectEqual(DispatchTimeInterval.microseconds(i), DispatchTimeInterval.nanoseconds(i * 1000))
	}


	// Check some cases that used to cause arithmetic overflow when evaluating the rawValue for ==
	var t = DispatchTimeInterval.seconds(Int.max)
	expectTrue(t == t) // This would crash.

	t = DispatchTimeInterval.seconds(-Int.max)
	expectTrue(t == t) // This would crash.

	t = DispatchTimeInterval.milliseconds(Int.max)
	expectTrue(t == t) // This would crash.

	t = DispatchTimeInterval.milliseconds(-Int.max)
	expectTrue(t == t) // This would crash.

	t = DispatchTimeInterval.microseconds(Int.max)
	expectTrue(t == t) // This would crash.

	t = DispatchTimeInterval.microseconds(-Int.max)
	expectTrue(t == t) // This would crash.
}

DispatchAPI.test("DispatchTimeInterval.never.equals") {
	expectTrue(DispatchTimeInterval.never == DispatchTimeInterval.never)
	expectTrue(DispatchTimeInterval.seconds(10) != DispatchTimeInterval.never);
	expectTrue(DispatchTimeInterval.never != DispatchTimeInterval.seconds(10));
	expectTrue(DispatchTimeInterval.seconds(10) == DispatchTimeInterval.seconds(10));
}
