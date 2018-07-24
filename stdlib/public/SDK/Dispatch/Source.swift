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

// import Foundation
import _SwiftDispatchOverlayShims

public extension DispatchSourceProtocol {
	typealias DispatchSourceHandler = @convention(block) () -> Void

	public func setEventHandler(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], handler: DispatchSourceHandler?) {
		if #available(macOS 10.10, iOS 8.0, *),
                   let h = handler,
                   qos != .unspecified || !flags.isEmpty {
			let item = DispatchWorkItem(qos: qos, flags: flags, block: h)
			_swift_dispatch_source_set_event_handler(self as! DispatchSource, item._block)
		} else {
			_swift_dispatch_source_set_event_handler(self as! DispatchSource, handler)
		}
	}

	@available(macOS 10.10, iOS 8.0, *)
	public func setEventHandler(handler: DispatchWorkItem) {
		_swift_dispatch_source_set_event_handler(self as! DispatchSource, handler._block)
	}

	public func setCancelHandler(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], handler: DispatchSourceHandler?) {
		if #available(macOS 10.10, iOS 8.0, *),
                   let h = handler,
                   qos != .unspecified || !flags.isEmpty {
			let item = DispatchWorkItem(qos: qos, flags: flags, block: h)
			_swift_dispatch_source_set_cancel_handler(self as! DispatchSource, item._block)
		} else {
			_swift_dispatch_source_set_cancel_handler(self as! DispatchSource, handler)
		}
	}

	@available(macOS 10.10, iOS 8.0, *)
	public func setCancelHandler(handler: DispatchWorkItem) {
		_swift_dispatch_source_set_cancel_handler(self as! DispatchSource, handler._block)
	}

	public func setRegistrationHandler(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], handler: DispatchSourceHandler?) {
		if #available(macOS 10.10, iOS 8.0, *),
                   let h = handler,
                   qos != .unspecified || !flags.isEmpty {
			let item = DispatchWorkItem(qos: qos, flags: flags, block: h)
			_swift_dispatch_source_set_registration_handler(self as! DispatchSource, item._block)
		} else {
			_swift_dispatch_source_set_registration_handler(self as! DispatchSource, handler)
		}
	}

	@available(macOS 10.10, iOS 8.0, *)
	public func setRegistrationHandler(handler: DispatchWorkItem) {
		_swift_dispatch_source_set_registration_handler(self as! DispatchSource, handler._block)
	}

	@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
	public func activate() {
		(self as! DispatchSource).activate()
	}

	public func cancel() {
		__dispatch_source_cancel(self as! DispatchSource)
	}

	public func resume() {
		(self as! DispatchSource).resume()
	}

	public func suspend() {
		(self as! DispatchSource).suspend()
	}

	public var handle: UInt {
		return __dispatch_source_get_handle(self as! DispatchSource)
	}

	public var mask: UInt {
		return __dispatch_source_get_mask(self as! DispatchSource)
	}

	public var data: UInt {
		return __dispatch_source_get_data(self as! DispatchSource)
	}

	public var isCancelled: Bool {
		return __dispatch_source_testcancel(self as! DispatchSource) != 0
	}
}

public extension DispatchSource {
	public struct MachSendEvent : OptionSet, RawRepresentable {
		public let rawValue: UInt
		public init(rawValue: UInt) { self.rawValue = rawValue }

		public static let dead = MachSendEvent(rawValue: 0x1)
	}

	public struct MemoryPressureEvent : OptionSet, RawRepresentable {
		public let rawValue: UInt
		public init(rawValue: UInt) { self.rawValue = rawValue }

		public static let normal = MemoryPressureEvent(rawValue: 0x1)
		public static let warning = MemoryPressureEvent(rawValue: 0x2)
		public static let critical = MemoryPressureEvent(rawValue: 0x4)
		public static let all: MemoryPressureEvent = [.normal, .warning, .critical]
	}

	public struct ProcessEvent : OptionSet, RawRepresentable {
		public let rawValue: UInt
		public init(rawValue: UInt) { self.rawValue = rawValue }

		public static let exit = ProcessEvent(rawValue: 0x80000000)
		public static let fork = ProcessEvent(rawValue: 0x40000000)
		public static let exec = ProcessEvent(rawValue: 0x20000000)
		public static let signal = ProcessEvent(rawValue: 0x08000000)
		public static let all: ProcessEvent = [.exit, .fork, .exec, .signal]
	}

	public struct TimerFlags : OptionSet, RawRepresentable {
		public let rawValue: UInt
		public init(rawValue: UInt) { self.rawValue = rawValue }

		public static let strict = TimerFlags(rawValue: 1)
	}

	public struct FileSystemEvent : OptionSet, RawRepresentable {
		public let rawValue: UInt
		public init(rawValue: UInt) { self.rawValue = rawValue }

		public static let delete = FileSystemEvent(rawValue: 0x1)
		public static let write = FileSystemEvent(rawValue: 0x2)
		public static let extend = FileSystemEvent(rawValue: 0x4)
		public static let attrib = FileSystemEvent(rawValue: 0x8)
		public static let link = FileSystemEvent(rawValue: 0x10)
		public static let rename = FileSystemEvent(rawValue: 0x20)
		public static let revoke = FileSystemEvent(rawValue: 0x40)
		public static let funlock = FileSystemEvent(rawValue: 0x100)

		public static let all: FileSystemEvent = [
			.delete, .write, .extend, .attrib, .link, .rename, .revoke]
	}

	public class func makeMachSendSource(port: mach_port_t, eventMask: MachSendEvent, queue: DispatchQueue? = nil) -> DispatchSourceMachSend {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_MACH_SEND(), UInt(port), eventMask.rawValue, queue) as DispatchSourceMachSend
	}

	public class func makeMachReceiveSource(port: mach_port_t, queue: DispatchQueue? = nil) -> DispatchSourceMachReceive {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_MACH_RECV(), UInt(port), 0, queue) as DispatchSourceMachReceive
	}

	public class func makeMemoryPressureSource(eventMask: MemoryPressureEvent, queue: DispatchQueue? = nil) -> DispatchSourceMemoryPressure {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_MEMORYPRESSURE(), 0, eventMask.rawValue, queue) as DispatchSourceMemoryPressure
	}

	public class func makeProcessSource(identifier: pid_t, eventMask: ProcessEvent, queue: DispatchQueue? = nil) -> DispatchSourceProcess {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_PROC(), UInt(identifier), eventMask.rawValue, queue) as DispatchSourceProcess
	}

	public class func makeReadSource(fileDescriptor: Int32, queue: DispatchQueue? = nil) -> DispatchSourceRead {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_READ(), UInt(fileDescriptor), 0, queue) as DispatchSourceRead
	}

	public class func makeSignalSource(signal: Int32, queue: DispatchQueue? = nil) -> DispatchSourceSignal {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_SIGNAL(), UInt(signal), 0, queue) as DispatchSourceSignal
	}

	public class func makeTimerSource(flags: TimerFlags = [], queue: DispatchQueue? = nil) -> DispatchSourceTimer {
		return _swift_dispatch_source_create(_swift_dispatch_source_type_TIMER(), 0, flags.rawValue, queue) as DispatchSourceTimer
	}

	public class func makeUserDataAddSource(queue: DispatchQueue? = nil) -> DispatchSourceUserDataAdd {
		return _swift_dispatch_source_create(_swift_dispatch_source_type_DATA_ADD(), 0, 0, queue) as DispatchSourceUserDataAdd
	}

	public class func makeUserDataOrSource(queue: DispatchQueue? = nil) -> DispatchSourceUserDataOr {
		return _swift_dispatch_source_create(_swift_dispatch_source_type_DATA_OR(), 0, 0, queue) as DispatchSourceUserDataOr
	}

	public class func makeUserDataReplaceSource(queue: DispatchQueue? = nil) -> DispatchSourceUserDataReplace {
		return _swift_dispatch_source_create(_swift_dispatch_source_type_DATA_REPLACE(), 0, 0, queue) as DispatchSourceUserDataReplace
	}

	public class func makeFileSystemObjectSource(
		fileDescriptor: Int32, eventMask: FileSystemEvent, queue: DispatchQueue? = nil) -> DispatchSourceFileSystemObject 
	{
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_VNODE(), UInt(fileDescriptor), eventMask.rawValue, queue) as DispatchSourceFileSystemObject
	}

	public class func makeWriteSource(fileDescriptor: Int32, queue: DispatchQueue? = nil) -> DispatchSourceWrite {
		return _swift_dispatch_source_create(
			_swift_dispatch_source_type_WRITE(), UInt(fileDescriptor), 0, queue) as DispatchSourceWrite
	}
}

public extension DispatchSourceMachSend {
	public var handle: mach_port_t {
		return mach_port_t(__dispatch_source_get_handle(self as! DispatchSource))
	}

	public var data: DispatchSource.MachSendEvent {
		let data = __dispatch_source_get_data(self as! DispatchSource)
		return DispatchSource.MachSendEvent(rawValue: data)
	}

	public var mask: DispatchSource.MachSendEvent {
		let mask = __dispatch_source_get_mask(self as! DispatchSource)
		return DispatchSource.MachSendEvent(rawValue: mask)
	}
}

public extension DispatchSourceMachReceive {
	public var handle: mach_port_t {
		return mach_port_t(__dispatch_source_get_handle(self as! DispatchSource))
	}
}

public extension DispatchSourceMemoryPressure {
	public var data: DispatchSource.MemoryPressureEvent {
		let data = __dispatch_source_get_data(self as! DispatchSource)
		return DispatchSource.MemoryPressureEvent(rawValue: data)
	}

	public var mask: DispatchSource.MemoryPressureEvent {
		let mask = __dispatch_source_get_mask(self as! DispatchSource)
		return DispatchSource.MemoryPressureEvent(rawValue: mask)
	}
}

public extension DispatchSourceProcess {
	public var handle: pid_t {
		return pid_t(__dispatch_source_get_handle(self as! DispatchSource))
	}

	public var data: DispatchSource.ProcessEvent {
		let data = __dispatch_source_get_data(self as! DispatchSource)
		return DispatchSource.ProcessEvent(rawValue: data)
	}

	public var mask: DispatchSource.ProcessEvent {
		let mask = __dispatch_source_get_mask(self as! DispatchSource)
		return DispatchSource.ProcessEvent(rawValue: mask)
	}
}

public extension DispatchSourceTimer {
	///
	/// Sets the deadline and leeway for a timer event that fires once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared and the next timer event will occur at `deadline`.
	///
	/// Delivery of the timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	/// - note: Delivery of the timer event does not cancel the timer source.
	///
	/// - parameter deadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on Mach absolute
	///     time.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(deadline:repeating:leeway:)")
	public func scheduleOneshot(deadline: DispatchTime, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, UInt64(deadline.rawValue), ~0, UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline and leeway for a timer event that fires once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared and the next timer event will occur at `wallDeadline`.
	///
	/// Delivery of the timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	/// - note: Delivery of the timer event does not cancel the timer source.
	///
	/// - parameter wallDeadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on
	///     `gettimeofday(3)`.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(wallDeadline:repeating:leeway:)")
	public func scheduleOneshot(wallDeadline: DispatchWallTime, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, UInt64(wallDeadline.rawValue), ~0, UInt64(leeway.rawValue))
	}
	///
	/// Sets the deadline, interval and leeway for a timer event that fires at least once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `deadline` and every `interval` units of
	/// time thereafter until the timer source is canceled.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `deadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `deadline + N * interval`, the upper
	/// limit is the smaller of `leeway` and `interval/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter deadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on Mach absolute
	///     time.
	/// - parameter interval: the interval for the timer.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(deadline:repeating:leeway:)")
	public func scheduleRepeating(deadline: DispatchTime, interval: DispatchTimeInterval, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, deadline.rawValue, interval == .never ? ~0 : UInt64(interval.rawValue), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, interval and leeway for a timer event that fires at least once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `deadline` and every `interval` seconds
	/// thereafter until the timer source is canceled.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption and
	/// system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `deadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `deadline + N * interval`, the upper
	/// limit is the smaller of `leeway` and `interval/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter deadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on Mach absolute
	///     time.
	/// - parameter interval: the interval for the timer in seconds.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(deadline:repeating:leeway:)")
	public func scheduleRepeating(deadline: DispatchTime, interval: Double, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, deadline.rawValue, interval.isInfinite ? ~0 : UInt64(interval * Double(NSEC_PER_SEC)), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, interval and leeway for a timer event that fires at least once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `wallDeadline` and every `interval` units of
	/// time thereafter until the timer source is canceled.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption and
	/// system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `wallDeadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `wallDeadline + N * interval`, the upper
	/// limit is the smaller of `leeway` and `interval/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter wallDeadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on
	///     `gettimeofday(3)`.
	/// - parameter interval: the interval for the timer.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(wallDeadline:repeating:leeway:)")
	public func scheduleRepeating(wallDeadline: DispatchWallTime, interval: DispatchTimeInterval, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, wallDeadline.rawValue, interval == .never ? ~0 : UInt64(interval.rawValue), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, interval and leeway for a timer event that fires at least once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `wallDeadline` and every `interval` seconds
	/// thereafter until the timer source is canceled.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption and
	/// system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `wallDeadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `wallDeadline + N * interval`, the upper
	/// limit is the smaller of `leeway` and `interval/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter wallDeadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on
	///     `gettimeofday(3)`.
	/// - parameter interval: the interval for the timer in seconds.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, deprecated: 4, renamed: "schedule(wallDeadline:repeating:leeway:)")
	public func scheduleRepeating(wallDeadline: DispatchWallTime, interval: Double, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, wallDeadline.rawValue, interval.isInfinite ? ~0 : UInt64(interval * Double(NSEC_PER_SEC)), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, repeat interval and leeway for a timer event.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `deadline` and every `repeating` units of
	/// time thereafter until the timer source is canceled. If the value of `repeating` is `.never`,
	/// or is defaulted, the timer fires only once.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `deadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `deadline + N * repeating`, the upper
	/// limit is the smaller of `leeway` and `repeating/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter deadline: the time at which the first timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on Mach absolute
	///     time.
	/// - parameter repeating: the repeat interval for the timer, or `.never` if the timer should fire
	///		only once.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, introduced: 4)
	public func schedule(deadline: DispatchTime, repeating interval: DispatchTimeInterval = .never, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, deadline.rawValue, interval == .never ? ~0 : UInt64(interval.rawValue), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, repeat interval and leeway for a timer event.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `deadline` and every `repeating` seconds
	/// thereafter until the timer source is canceled. If the value of `repeating` is `.infinity`,
	/// the timer fires only once.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `deadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `deadline + N * repeating`, the upper
	/// limit is the smaller of `leeway` and `repeating/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter deadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on Mach absolute
	///     time.
	/// - parameter repeating: the repeat interval for the timer in seconds, or `.infinity` if the timer
	///		should fire only once.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, introduced: 4)
	public func schedule(deadline: DispatchTime, repeating interval: Double, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, deadline.rawValue, interval.isInfinite ? ~0 : UInt64(interval * Double(NSEC_PER_SEC)), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, repeat interval and leeway for a timer event.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `wallDeadline` and every `repeating` units of
	/// time thereafter until the timer source is canceled. If the value of `repeating` is `.never`,
	/// or is defaulted, the timer fires only once.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption and
	/// system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `wallDeadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `wallDeadline + N * repeating`, the upper
	/// limit is the smaller of `leeway` and `repeating/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter wallDeadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on
	///     `gettimeofday(3)`.
	/// - parameter repeating: the repeat interval for the timer, or `.never` if the timer should fire
	///		only once.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, introduced: 4)
	public func schedule(wallDeadline: DispatchWallTime, repeating interval: DispatchTimeInterval = .never, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, wallDeadline.rawValue, interval == .never ? ~0 : UInt64(interval.rawValue), UInt64(leeway.rawValue))
	}

	///
	/// Sets the deadline, repeat interval and leeway for a timer event that fires at least once.
	///
	/// Once this function returns, any pending source data accumulated for the previous timer values
	/// has been cleared. The next timer event will occur at `wallDeadline` and every `repeating` seconds
	/// thereafter until the timer source is canceled. If the value of `repeating` is `.infinity`,
	/// the timer fires only once.
	///
	/// Delivery of a timer event may be delayed by the system in order to improve power consumption
	/// and system performance. The upper limit to the allowable delay may be configured with the `leeway`
	/// argument; the lower limit is under the control of the system.
	///
	/// For the initial timer fire at `wallDeadline`, the upper limit to the allowable delay is set to
	/// `leeway`. For the subsequent timer fires at `wallDeadline + N * repeating`, the upper
	/// limit is the smaller of `leeway` and `repeating/2`.
	///
	/// The lower limit to the allowable delay may vary with process state such as visibility of the
	/// application UI. If the timer source was created with flags `TimerFlags.strict`, the system
	/// will make a best effort to strictly observe the provided `leeway` value, even if it is smaller
	/// than the current lower limit. Note that a minimal amount of delay is to be expected even if
	/// this flag is specified.
	///
	/// Calling this method has no effect if the timer source has already been canceled.
	///
	/// - parameter wallDeadline: the time at which the timer event will be delivered, subject to the
	///     leeway and other considerations described above. The deadline is based on
	///     `gettimeofday(3)`.
	/// - parameter repeating: the repeat interval for the timer in secondss, or `.infinity` if the timer
	/// 	should fire only once.
	/// - parameter leeway: the leeway for the timer.
	///
	@available(swift, introduced: 4)
	public func schedule(wallDeadline: DispatchWallTime, repeating interval: Double, leeway: DispatchTimeInterval = .nanoseconds(0)) {
		__dispatch_source_set_timer(self as! DispatchSource, wallDeadline.rawValue, interval.isInfinite ? ~0 : UInt64(interval * Double(NSEC_PER_SEC)), UInt64(leeway.rawValue))
	}
}

public extension DispatchSourceFileSystemObject {
	public var handle: Int32 {
		return Int32(__dispatch_source_get_handle(self as! DispatchSource))
	}

	public var data: DispatchSource.FileSystemEvent {
		let data = __dispatch_source_get_data(self as! DispatchSource)
		return DispatchSource.FileSystemEvent(rawValue: data)
	}

	public var mask: DispatchSource.FileSystemEvent {
		let data = __dispatch_source_get_mask(self as! DispatchSource)
		return DispatchSource.FileSystemEvent(rawValue: data)
	}
}

public extension DispatchSourceUserDataAdd {
	/// @function add
	///
	/// @abstract
	/// Merges data into a dispatch source of type DISPATCH_SOURCE_TYPE_DATA_ADD
	/// and submits its event handler block to its target queue.
	///
	/// @param data
	/// The value to add to the current pending data. A value of zero has no effect
	/// and will not result in the submission of the event handler block.
	public func add(data: UInt) {
		__dispatch_source_merge_data(self as! DispatchSource, data)
	}
}

public extension DispatchSourceUserDataOr {
	/// @function or
	///
	/// @abstract
	/// Merges data into a dispatch source of type DISPATCH_SOURCE_TYPE_DATA_OR and
	/// submits its event handler block to its target queue.
	///
	/// @param data
	/// The value to OR into the current pending data. A value of zero has no effect
	/// and will not result in the submission of the event handler block.
	public func or(data: UInt) {
		__dispatch_source_merge_data(self as! DispatchSource, data)
	}
}

public extension DispatchSourceUserDataReplace {
	/// @function replace
	///
	/// @abstract
	/// Merges data into a dispatch source of type DISPATCH_SOURCE_TYPE_DATA_REPLACE
	/// and submits its event handler block to its target queue.
	///
	/// @param data
	/// The value that will replace the current pending data. A value of zero will be stored
	/// but will not result in the submission of the event handler block.
	public func replace(data: UInt) {
		__dispatch_source_merge_data(self as! DispatchSource, data)
	}
}
