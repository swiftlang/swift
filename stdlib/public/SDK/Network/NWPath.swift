//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Foundation
import _SwiftNetworkOverlayShims

/// An NWInterface object represents an instance of a network interface of a specific
/// type, such as a Wi-Fi or Cellular interface.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public struct NWInterface : Hashable, CustomDebugStringConvertible {

	public var debugDescription: String {
		return self.name
	}

	public static func ==(lhs: NWInterface, rhs: NWInterface) -> Bool {
		return lhs.index == rhs.index && lhs.name == rhs.name
	}

	public func hash(into hasher: inout Hasher) {
		hasher.combine(self.index)
		hasher.combine(self.name)
	}

	/// Interface types represent the underlying media for a network link
	public enum InterfaceType {
		/// A virtual or otherwise unknown interface type
		case other
		/// A Wi-Fi link
		case wifi
		/// A Cellular link
		case cellular
		/// A Wired Ethernet link
		case wiredEthernet
		/// The Loopback Interface
		case loopback

		internal var nw : nw_interface_type_t {
			switch self {
			case .wifi:
				return Network.nw_interface_type_wifi
			case .cellular:
				return Network.nw_interface_type_cellular
			case .wiredEthernet:
				return Network.nw_interface_type_wired
			case .loopback:
				return Network.nw_interface_type_loopback
			case .other:
				return Network.nw_interface_type_other
			}
		}

		internal init(_ nw: nw_interface_type_t) {
			switch nw {
			case Network.nw_interface_type_wifi:
				self = .wifi
			case Network.nw_interface_type_cellular:
				self = .cellular
			case Network.nw_interface_type_wired:
				self = .wiredEthernet
			case Network.nw_interface_type_loopback:
				self = .loopback
			default:
				self = .other
			}
		}
	}

	public let type: InterfaceType

	/// The name of the interface, such as "en0"
	public let name: String

	/// The kernel index of the interface
	public let index: Int

	internal let nw: nw_interface_t

	internal init(_ nw: nw_interface_t) {
		self.nw = nw
		self.type = NWInterface.InterfaceType(nw_interface_get_type(nw))
		self.name = String(cString: nw_interface_get_name(nw))
		self.index = Int(nw_interface_get_index(nw))
	}

	internal init?(_ index: Int) {
		guard let nw = nw_interface_create_with_index(UInt32(index)) else {
			return nil
		}
		self.init(nw)
	}

	internal init?(_ name: String) {
		guard let nw = nw_interface_create_with_name(name) else {
			return nil
		}
		self.init(nw)
	}
}

/// An NWPath object represents a snapshot of network path state. This state
/// represents the known information about the local interface and routes that may
/// be used to send and receive data. If the network path for a connection changes
/// due to interface characteristics, addresses, or other attributes, a new NWPath
/// object will be generated. Note that the differences in the path attributes may not
/// be visible through public accessors, and these changes should be treated merely
/// as an indication that something about the network has changed.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public struct NWPath : Equatable, CustomDebugStringConvertible {

	public var debugDescription: String {
		return String(describing: self.nw)
	}

	/// An NWPath status indicates if there is a usable route available upon which to send and receive data.
	public enum Status {
		/// The path has a usable route upon which to send and receive data
		case satisfied
		/// The path does not have a usable route. This may be due to a network interface being down, or due to system policy.
		case unsatisfied
		/// The path does not currently have a usable route, but a connection attempt will trigger network attachment.
		case requiresConnection
	}

	public let status: NWPath.Status

	/// A list of all interfaces currently available to this path
	public let availableInterfaces: [NWInterface]

	/// Checks if the path uses an NWInterface that is considered to be expensive
	///
	/// Cellular interfaces are considered expensive. WiFi hotspots from an iOS device are considered expensive. Other
	/// interfaces may appear as expensive in the future.
	public let isExpensive : Bool
	public let supportsIPv4 : Bool
	public let supportsIPv6 : Bool
	public let supportsDNS : Bool

	/// Check the local endpoint set on a path. This will be nil for paths
	/// from an NWPathMonitor. For paths from an NWConnection, this will
	/// be set to the local address and port in use by the connection.
	public let localEndpoint: NWEndpoint?

	/// Check the remote endpoint set on a path. This will be nil for paths
	/// from an NWPathMonitor. For paths from an NWConnection, this will
	/// be set to the remote address and port in use by the connection.
	public let remoteEndpoint: NWEndpoint?

	/// Checks if the path uses an NWInterface with the specified type
	public func usesInterfaceType(_ type: NWInterface.InterfaceType) -> Bool {
		if let path = self.nw {
			return nw_path_uses_interface_type(path, type.nw)
		}
		return false
	}

	internal let nw: nw_path_t?

	internal init(_ path: nw_path_t?) {
		var interfaces = [NWInterface]()
		var local: NWEndpoint? = nil
		var remote: NWEndpoint? = nil
		if let path = path {
			let nwstatus = nw_path_get_status(path)
			switch (nwstatus) {
			case Network.nw_path_status_satisfied:
				self.status = .satisfied
			case Network.nw_path_status_satisfiable:
				self.status = .requiresConnection
			default:
				self.status = .unsatisfied
			}
			self.isExpensive = nw_path_is_expensive(path)
			self.supportsDNS = nw_path_has_dns(path)
			self.supportsIPv4 = nw_path_has_ipv4(path)
			self.supportsIPv6 = nw_path_has_ipv6(path)

			nw_path_enumerate_interfaces(path, { (interface) in
				interfaces.append(NWInterface(interface))
				return true
			})

			if let nwlocal = nw_path_copy_effective_local_endpoint(path) {
				local = NWEndpoint(nwlocal)
			}
			if let nwremote = nw_path_copy_effective_remote_endpoint(path) {
				remote = NWEndpoint(nwremote)
			}
		} else {
			self.status = .unsatisfied
			self.isExpensive = false
			self.supportsDNS = false
			self.supportsIPv4 = false
			self.supportsIPv6 = false
		}
		self.availableInterfaces = interfaces
		self.nw = path
		self.localEndpoint = local
		self.remoteEndpoint = remote
	}

	public static func ==(lhs: NWPath, rhs: NWPath) -> Bool {
		if let lnw = lhs.nw, let rnw = rhs.nw {
			return nw_path_is_equal(lnw, rnw)
		}
		return lhs.nw == nil && rhs.nw == nil
	}
}

/// The NWPathMonitor allows the caller to fetch the current global path (or
/// a path restricted to a specific network interface type). The path for the monitor
/// is an observable property that will be updated upon each network change.
/// Paths generated by a path monitor are not specific to a given endpoint, and
/// will not have the localEndpoint or remoteEndpoint properties set.
/// The paths will watch the state of multiple interfaces, and allows the
/// application to enumerate the available interfaces for use in creating connections
/// or listeners bound to specific interfaces.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public final class NWPathMonitor {

	/// Access the current network path tracked by the monitor
	public var currentPath = NWPath(nil)
	fileprivate let nw : nw_path_monitor_t

	/// Set a block to be called when the network path changes
	private var _pathUpdateHandler: ((_ newPath: NWPath) -> Void)?
	public var pathUpdateHandler: ((_ newPath: NWPath) -> Void)? {
		set {
			self._pathUpdateHandler = newValue
		}
		get {
			return self._pathUpdateHandler
		}
	}

	/// Start the path monitor and set a queue on which path updates
	/// will be delivered.
	/// Start should only be called once on a monitor, and multiple calls to start will
	/// be ignored.
	public func start(queue: DispatchQueue) {
		self._queue = queue
		nw_path_monitor_set_queue(self.nw, queue)
		nw_path_monitor_start(self.nw)
	}

	/// Cancel the path monitor, after which point no more path updates will
	/// be delivered.
	public func cancel() {
		nw_path_monitor_cancel(self.nw)
	}

	private var _queue: DispatchQueue?

	/// Get queue used for delivering the pathUpdateHandler block.
	/// If the path monitor has not yet been started, the queue will be nil. Once the
	/// path monitor has been started, the queue will be valid.
	public var queue: DispatchQueue? {
		get {
			return self._queue
		}
	}


	/// Create a network path monitor to monitor overall network state for the
	/// system. This allows enumeration of all interfaces that are available for
	/// general use by the application.
	public init() {
		self.nw = nw_path_monitor_create()
		nw_path_monitor_set_update_handler(self.nw) { (newPath) in
			self.currentPath = NWPath(newPath)
			if let handler = self._pathUpdateHandler {
				handler(self.currentPath)
			}
		}
	}

	/// Create a network path monitor that watches a single interface type.
	public init(requiredInterfaceType: NWInterface.InterfaceType) {
		self.nw = nw_path_monitor_create_with_type(requiredInterfaceType.nw)
		nw_path_monitor_set_update_handler(self.nw) { (newPath) in
			self.currentPath = NWPath(newPath)
			if let handler = self._pathUpdateHandler {
				handler(self.currentPath)
			}
		}
	}
}
