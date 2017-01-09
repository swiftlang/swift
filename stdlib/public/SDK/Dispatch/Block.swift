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

public struct DispatchWorkItemFlags : OptionSet, RawRepresentable {
	public let rawValue: UInt
	public init(rawValue: UInt) { self.rawValue = rawValue }

	public static let barrier = DispatchWorkItemFlags(rawValue: 0x1)

	@available(OSX 10.10, iOS 8.0, *)
	public static let detached = DispatchWorkItemFlags(rawValue: 0x2)

	@available(OSX 10.10, iOS 8.0, *)
	public static let assignCurrentContext = DispatchWorkItemFlags(rawValue: 0x4)

	@available(OSX 10.10, iOS 8.0, *)
	public static let noQoS = DispatchWorkItemFlags(rawValue: 0x8)

	@available(OSX 10.10, iOS 8.0, *)
	public static let inheritQoS = DispatchWorkItemFlags(rawValue: 0x10)

	@available(OSX 10.10, iOS 8.0, *)
	public static let enforceQoS = DispatchWorkItemFlags(rawValue: 0x20)
}

@available(OSX 10.10, iOS 8.0, *)
public class DispatchWorkItem {
	internal var _block: _DispatchBlock

	public init(qos: DispatchQoS = .unspecified, flags: DispatchWorkItemFlags = [], block: @escaping @convention(block) () -> Void) {
		_block =  _swift_dispatch_block_create_with_qos_class(
			__dispatch_block_flags_t(rawValue: flags.rawValue),
			qos.qosClass.rawValue, Int32(qos.relativePriority), block)
	}

	// Used by DispatchQueue.synchronously<T> to provide a path through
	// dispatch_block_t, as we know the lifetime of the block in question.
	internal init(flags: DispatchWorkItemFlags = [], noescapeBlock: () -> Void) {
		_block = _swift_dispatch_block_create_noescape(
			__dispatch_block_flags_t(rawValue: flags.rawValue), noescapeBlock)
	}

	public func perform() {
		_block()
	}

	public func wait() {
		_ = _swift_dispatch_block_wait(_block, DispatchTime.distantFuture.rawValue)
	}

	public func wait(timeout: DispatchTime) -> DispatchTimeoutResult {
		return _swift_dispatch_block_wait(_block, timeout.rawValue) == 0 ? .success : .timedOut
	}

	public func wait(wallTimeout: DispatchWallTime) -> DispatchTimeoutResult {
		return _swift_dispatch_block_wait(_block, wallTimeout.rawValue) == 0 ? .success : .timedOut
	}

	public func notify(
		qos: DispatchQoS = .unspecified, 
		flags: DispatchWorkItemFlags = [], 
		queue: DispatchQueue, 
		execute: @escaping @convention(block) () -> Void) 
	{
		if qos != .unspecified || !flags.isEmpty {
			let item = DispatchWorkItem(qos: qos, flags: flags, block: execute)
			_swift_dispatch_block_notify(_block, queue, item._block)
		} else {
			_swift_dispatch_block_notify(_block, queue, execute)
		}
	}

	public func notify(queue: DispatchQueue, execute: DispatchWorkItem) {
		_swift_dispatch_block_notify(_block, queue, execute._block)
	}

	public func cancel() {
		_swift_dispatch_block_cancel(_block)
	}

	public var isCancelled: Bool {
		return _swift_dispatch_block_testcancel(_block) != 0
	}
}

/// The dispatch_block_t typealias is different from usual closures in that it
/// uses @convention(block). This is to avoid unnecessary bridging between
/// C blocks and Swift closures, which interferes with dispatch APIs that depend
/// on the referential identity of a block. Particularly, dispatch_block_create.
internal typealias _DispatchBlock = @convention(block) () -> Void

