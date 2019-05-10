
//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import _SwiftDispatchOverlayShims

public struct DispatchTime : Comparable {
	private static let timebaseInfo: mach_timebase_info_data_t = {
		var info = mach_timebase_info_data_t(numer: 1, denom: 1)
		mach_timebase_info(&info)
		return info
	}()
 
	public let rawValue: dispatch_time_t

	public static func now() -> DispatchTime {
		let t = __dispatch_time(0, 0)
		return DispatchTime(rawValue: t)
	}

	public static let distantFuture = DispatchTime(rawValue: ~0)

	fileprivate init(rawValue: dispatch_time_t) { 
		self.rawValue = rawValue
	}

	/// Creates a `DispatchTime` relative to the system clock that
	/// ticks since boot.
	///
	/// - Parameters:
	///   - uptimeNanoseconds: The number of nanoseconds since boot, excluding
	///                        time the system spent asleep
	/// - Returns: A new `DispatchTime`
	/// - Discussion: This clock is the same as the value returned by
	///               `mach_absolute_time` when converted into nanoseconds.
	///               On some platforms, the nanosecond value is rounded up to a
	///               multiple of the Mach timebase, using the conversion factors
	///               returned by `mach_timebase_info()`. The nanosecond equivalent
	///               of the rounded result can be obtained by reading the
	///               `uptimeNanoseconds` property.
	///               Note that `DispatchTime(uptimeNanoseconds: 0)` is
	///               equivalent to `DispatchTime.now()`, that is, its value
	///               represents the number of nanoseconds since boot (excluding
	///               system sleep time), not zero nanoseconds since boot.
	public init(uptimeNanoseconds: UInt64) {
		var rawValue = uptimeNanoseconds

		// UInt64.max means distantFuture. Do not try to scale it.
		if rawValue != UInt64.max && DispatchTime.timebaseInfo.numer != DispatchTime.timebaseInfo.denom {
			var (result, overflow) = rawValue.multipliedReportingOverflow(by: UInt64(DispatchTime.timebaseInfo.denom))
			if !overflow {
				(result, overflow) = result.addingReportingOverflow(UInt64(DispatchTime.timebaseInfo.numer - 1))
			}
			rawValue = overflow ? UInt64.max : result / UInt64(DispatchTime.timebaseInfo.numer)
		}
		self.rawValue = dispatch_time_t(rawValue)
	}

	public var uptimeNanoseconds: UInt64 {
		var result = self.rawValue
		var overflow: Bool

		// UInt64.max means distantFuture. Do not try to scale it.
		if rawValue != UInt64.max && DispatchTime.timebaseInfo.numer != DispatchTime.timebaseInfo.denom {
			(result, overflow) = result.multipliedReportingOverflow(by: UInt64(DispatchTime.timebaseInfo.numer))
			result = overflow ? UInt64.max : result / UInt64(DispatchTime.timebaseInfo.denom)
		}
		return result
	}
}

extension DispatchTime {
  public static func < (a: DispatchTime, b: DispatchTime) -> Bool {
    return a.rawValue < b.rawValue
  }

  public static func ==(a: DispatchTime, b: DispatchTime) -> Bool {
    return a.rawValue == b.rawValue
  }
}

public struct DispatchWallTime : Comparable {
	public let rawValue: dispatch_time_t

	public static func now() -> DispatchWallTime {
		return DispatchWallTime(rawValue: __dispatch_walltime(nil, 0))
	}

	public static let distantFuture = DispatchWallTime(rawValue: ~0)

	fileprivate init(rawValue: dispatch_time_t) {
		self.rawValue = rawValue
	}

	public init(timespec: timespec) {
		var t = timespec
		self.rawValue = __dispatch_walltime(&t, 0)
	}
}

extension DispatchWallTime {
  public static func <(a: DispatchWallTime, b: DispatchWallTime) -> Bool {
    let negativeOne: dispatch_time_t = ~0
    if b.rawValue == negativeOne {
      return a.rawValue != negativeOne
    } else if a.rawValue == negativeOne {
      return false
    }
    return -Int64(bitPattern: a.rawValue) < -Int64(bitPattern: b.rawValue)
  }

  public static func ==(a: DispatchWallTime, b: DispatchWallTime) -> Bool {
    return a.rawValue == b.rawValue
  }
}


// Returns m1 * m2, clamped to the range [Int64.min, Int64.max].
// Because of the way this function is used, we can always assume
// that m2 > 0.
private func clampedInt64Product(_ m1: Int64, _ m2: Int64) -> Int64 {
	assert(m2 > 0, "multiplier must be positive")
	let (result, overflow) = m1.multipliedReportingOverflow(by: m2)
	if overflow {
		return m1 > 0 ? Int64.max : Int64.min
	}
	return result
}

// Returns its argument clamped to the range [Int64.min, Int64.max].
private func toInt64Clamped(_ value: Double) -> Int64 {
	if value.isNaN { return Int64.max }
	if value >= Double(Int64.max) { return Int64.max }
	if value <= Double(Int64.min) { return Int64.min }
	return Int64(value)
}

/// Represents a time interval that can be used as an offset from a `DispatchTime`
/// or `DispatchWallTime`.
///
/// For example:
///		let inOneSecond = DispatchTime.now() + DispatchTimeInterval.seconds(1)
///
///	If the requested time interval is larger then the internal representation
/// permits, the result of adding it to a `DispatchTime` or `DispatchWallTime`
/// is `DispatchTime.distantFuture` and `DispatchWallTime.distantFuture`
/// respectively. Such time intervals compare as equal:
///
///		let t1 = DispatchTimeInterval.seconds(Int.max)
///		let t2 = DispatchTimeInterval.milliseconds(Int.max)
///		let result = t1 == t2   // true
public enum DispatchTimeInterval : Equatable {
	case seconds(Int)
	case milliseconds(Int)
	case microseconds(Int)
	case nanoseconds(Int)
	case never

	internal var rawValue: Int64 {
		switch self {
		case .seconds(let s): return clampedInt64Product(Int64(s), Int64(NSEC_PER_SEC))
		case .milliseconds(let ms): return clampedInt64Product(Int64(ms), Int64(NSEC_PER_MSEC))
		case .microseconds(let us): return clampedInt64Product(Int64(us), Int64(NSEC_PER_USEC))
		case .nanoseconds(let ns): return Int64(ns)
		case .never: return Int64.max
		}
	}

	public static func ==(lhs: DispatchTimeInterval, rhs: DispatchTimeInterval) -> Bool {
		switch (lhs, rhs) {
		case (.never, .never): return true
		case (.never, _): return false
		case (_, .never): return false
		default: return lhs.rawValue == rhs.rawValue
		}
	}
}

public func +(time: DispatchTime, interval: DispatchTimeInterval) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, interval.rawValue)
	return DispatchTime(rawValue: t)
}

public func -(time: DispatchTime, interval: DispatchTimeInterval) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, -interval.rawValue)
	return DispatchTime(rawValue: t)
}

public func +(time: DispatchTime, seconds: Double) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, toInt64Clamped(seconds * Double(NSEC_PER_SEC)));
	return DispatchTime(rawValue: t)
}

public func -(time: DispatchTime, seconds: Double) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, toInt64Clamped(-seconds * Double(NSEC_PER_SEC)));
	return DispatchTime(rawValue: t)
}

public func +(time: DispatchWallTime, interval: DispatchTimeInterval) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, interval.rawValue)
	return DispatchWallTime(rawValue: t)
}

public func -(time: DispatchWallTime, interval: DispatchTimeInterval) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, -interval.rawValue)
	return DispatchWallTime(rawValue: t)
}

public func +(time: DispatchWallTime, seconds: Double) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, toInt64Clamped(seconds * Double(NSEC_PER_SEC)));
	return DispatchWallTime(rawValue: t)
}

public func -(time: DispatchWallTime, seconds: Double) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, toInt64Clamped(-seconds * Double(NSEC_PER_SEC)));
	return DispatchWallTime(rawValue: t)
}
