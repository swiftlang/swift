//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// dispatch/time.h
// DISPATCH_TIME_NOW: ok
// DISPATCH_TIME_FOREVER: ok

public struct DispatchTime {
	public let rawValue: dispatch_time_t

	public static func now() -> DispatchTime {
		let t = __dispatch_time(0, 0)
		return DispatchTime(rawValue: t)
	}

	public static let distantFuture = DispatchTime(rawValue: ~0)

	private init(rawValue: dispatch_time_t) { 
		self.rawValue = rawValue
	}
}

public struct DispatchWallTime {
	public let rawValue: dispatch_time_t

	public static func now() -> DispatchWallTime {
		return DispatchWallTime(rawValue: __dispatch_walltime(nil, 0))
	}

	public static let distantFuture = DispatchWallTime(rawValue: ~0)

	private init(rawValue: dispatch_time_t) {
		self.rawValue = rawValue
	}

	public init(time: timespec) {
		var t = time
		self.rawValue = __dispatch_walltime(&t, 0)
	}
}

@available(*, deprecated, renamed: "DispatchWallTime")
public typealias DispatchWalltime = DispatchWallTime

public enum DispatchTimeInterval {
	case seconds(Int)
	case milliseconds(Int)
	case microseconds(Int)
	case nanoseconds(Int)

	internal var rawValue: UInt64 {
		switch self {
		case .seconds(let s): return UInt64(s) * NSEC_PER_SEC
		case .milliseconds(let ms): return UInt64(ms) * NSEC_PER_MSEC
		case .microseconds(let us): return UInt64(us) * NSEC_PER_USEC
		case .nanoseconds(let ns): return UInt64(ns)
		}
	}
}

public func +(time: DispatchTime, interval: DispatchTimeInterval) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, Int64(interval.rawValue))
	return DispatchTime(rawValue: t)
}

public func -(time: DispatchTime, interval: DispatchTimeInterval) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, -Int64(interval.rawValue))
	return DispatchTime(rawValue: t)
}

public func +(time: DispatchTime, seconds: Double) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, Int64(seconds * Double(NSEC_PER_SEC)))
	return DispatchTime(rawValue: t)
}

public func -(time: DispatchTime, seconds: Double) -> DispatchTime {
	let t = __dispatch_time(time.rawValue, Int64(-seconds * Double(NSEC_PER_SEC)))
	return DispatchTime(rawValue: t)
}

public func +(time: DispatchWallTime, interval: DispatchTimeInterval) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, Int64(interval.rawValue))
	return DispatchWallTime(rawValue: t)
}

public func -(time: DispatchWallTime, interval: DispatchTimeInterval) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, -Int64(interval.rawValue))
	return DispatchWallTime(rawValue: t)
}

public func +(time: DispatchWallTime, seconds: Double) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, Int64(seconds * Double(NSEC_PER_SEC)))
	return DispatchWallTime(rawValue: t)
}

public func -(time: DispatchWallTime, seconds: Double) -> DispatchWallTime {
	let t = __dispatch_time(time.rawValue, Int64(-seconds * Double(NSEC_PER_SEC)))
	return DispatchWallTime(rawValue: t)
}
