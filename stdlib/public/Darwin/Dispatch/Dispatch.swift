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

@_exported import Dispatch
import _SwiftDispatchOverlayShims

/// dispatch_assert

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
public enum DispatchPredicate {
	case onQueue(DispatchQueue)
	case onQueueAsBarrier(DispatchQueue)
	case notOnQueue(DispatchQueue)
}

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
public func _dispatchPreconditionTest(_ condition: DispatchPredicate) -> Bool {
	switch condition {
	case .onQueue(let q):
		__dispatch_assert_queue(q)
	case .onQueueAsBarrier(let q):
		__dispatch_assert_queue_barrier(q)
	case .notOnQueue(let q):
		__dispatch_assert_queue_not(q)
	}
	return true
}

@_transparent
@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
public func dispatchPrecondition(condition: @autoclosure () -> DispatchPredicate) {
	// precondition is able to determine release-vs-debug asserts where the overlay
	// cannot, so formulating this into a call that we can call with precondition()
	precondition(_dispatchPreconditionTest(condition()), "dispatchPrecondition failure")
}

/// qos_class_t

public struct DispatchQoS : Equatable {
	public let qosClass: QoSClass
	public let relativePriority: Int

	@available(macOS 10.10, iOS 8.0, *)
	public static let background = DispatchQoS(qosClass: .background, relativePriority: 0)

	@available(macOS 10.10, iOS 8.0, *)
	public static let utility = DispatchQoS(qosClass: .utility, relativePriority: 0)

	@available(macOS 10.10, iOS 8.0, *)
	public static let `default` = DispatchQoS(qosClass: .default, relativePriority: 0)

	@available(macOS 10.10, iOS 8.0, *)
	public static let userInitiated = DispatchQoS(qosClass: .userInitiated, relativePriority: 0)

	@available(macOS 10.10, iOS 8.0, *)
	public static let userInteractive = DispatchQoS(qosClass: .userInteractive, relativePriority: 0)

	public static let unspecified = DispatchQoS(qosClass: .unspecified, relativePriority: 0)

	public enum QoSClass {
		@available(macOS 10.10, iOS 8.0, *)
		case background

		@available(macOS 10.10, iOS 8.0, *)
		case utility

		@available(macOS 10.10, iOS 8.0, *)
		case `default`

		@available(macOS 10.10, iOS 8.0, *)
		case userInitiated

		@available(macOS 10.10, iOS 8.0, *)
		case userInteractive

		case unspecified

		@available(macOS 10.10, iOS 8.0, *)
		public init?(rawValue: qos_class_t) {
			switch rawValue {
			case QOS_CLASS_BACKGROUND: self = .background
			case QOS_CLASS_UTILITY: self = .utility
			case QOS_CLASS_DEFAULT: self = .default
			case QOS_CLASS_USER_INITIATED: self = .userInitiated
			case QOS_CLASS_USER_INTERACTIVE: self = .userInteractive
			case QOS_CLASS_UNSPECIFIED: self = .unspecified
			default: return nil
			}
		}

		@available(macOS 10.10, iOS 8.0, *)
		public var rawValue: qos_class_t {
			switch self {
			case .background: return QOS_CLASS_BACKGROUND
			case .utility: return QOS_CLASS_UTILITY
			case .default: return QOS_CLASS_DEFAULT
			case .userInitiated: return QOS_CLASS_USER_INITIATED
			case .userInteractive: return QOS_CLASS_USER_INTERACTIVE
			case .unspecified: return QOS_CLASS_UNSPECIFIED
			}
		}
	}

	public init(qosClass: QoSClass, relativePriority: Int) {
		self.qosClass = qosClass
		self.relativePriority = relativePriority
	}

	public static func ==(a: DispatchQoS, b: DispatchQoS) -> Bool {
		return a.qosClass == b.qosClass && a.relativePriority == b.relativePriority
	}
}

/// 
@_frozen
public enum DispatchTimeoutResult {
	case success
	case timedOut
}

/// dispatch_group

extension DispatchGroup {
	public func notify(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], queue: DispatchQueue, execute work: @escaping @convention(block) () -> Void) {
		if #available(macOS 10.10, iOS 8.0, *), qos != .unspecified || !flags.isEmpty {
			let item = DispatchWorkItem(qos: qos, flags: flags, block: work)
			_swift_dispatch_group_notify(self, queue, item._block)
		} else {
			_swift_dispatch_group_notify(self, queue, work)
		}
	}

	@available(macOS 10.10, iOS 8.0, *)
	public func notify(queue: DispatchQueue, work: DispatchWorkItem) {
		_swift_dispatch_group_notify(self, queue, work._block)
	}

	public func wait() {
		_ = __dispatch_group_wait(self, DispatchTime.distantFuture.rawValue)
	}

	public func wait(timeout: DispatchTime) -> DispatchTimeoutResult {
		return __dispatch_group_wait(self, timeout.rawValue) == 0 ? .success : .timedOut
	}

	public func wait(wallTimeout timeout: DispatchWallTime) -> DispatchTimeoutResult {
		return __dispatch_group_wait(self, timeout.rawValue) == 0 ? .success : .timedOut
	}
}

/// dispatch_semaphore

extension DispatchSemaphore {
	@discardableResult
	public func signal() -> Int {
		return __dispatch_semaphore_signal(self)
	}

	public func wait() {
		_ = __dispatch_semaphore_wait(self, DispatchTime.distantFuture.rawValue)
	}

	public func wait(timeout: DispatchTime) -> DispatchTimeoutResult {
		return __dispatch_semaphore_wait(self, timeout.rawValue) == 0 ? .success : .timedOut
	}

	public func wait(wallTimeout: DispatchWallTime) -> DispatchTimeoutResult {
		return __dispatch_semaphore_wait(self, wallTimeout.rawValue) == 0 ? .success : .timedOut
	}
}

