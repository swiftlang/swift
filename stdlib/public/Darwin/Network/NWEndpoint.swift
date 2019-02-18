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

import Darwin
import Foundation
import _SwiftNetworkOverlayShims

internal extension sockaddr_in {
	init(_ address:in_addr, _ port: in_port_t) {
		self.init(sin_len: UInt8(MemoryLayout<sockaddr_in>.size), sin_family: sa_family_t(AF_INET), sin_port: port,
				  sin_addr: address, sin_zero: (0, 0, 0, 0, 0, 0, 0, 0))
	}

	func withSockAddr<ReturnType>(_ body: (_ sa: UnsafePointer<sockaddr>) throws -> ReturnType) rethrows -> ReturnType {
		// We need to create a mutable copy of `self` so that we can pass it to `withUnsafePointer(to:_:)`.
		var sin = self
		return try withUnsafePointer(to: &sin) {
			try $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
				try body($0)
			}
		}
	}
}

internal extension sockaddr_in6 {
	init(_ address:in6_addr, _ port: in_port_t, flow: UInt32 = 0, scope: UInt32 = 0) {
		self.init(sin6_len: UInt8(MemoryLayout<sockaddr_in6>.size), sin6_family: sa_family_t(AF_INET6), sin6_port: port,
				  sin6_flowinfo: flow, sin6_addr: address, sin6_scope_id: scope)
	}

	func withSockAddr<ReturnType>(_ body: (_ sa: UnsafePointer<sockaddr>) throws -> ReturnType) rethrows -> ReturnType {
		// We need to create a mutable copy of `self` so that we can pass it to `withUnsafePointer(to:_:)`.
		var sin6 = self
		return try withUnsafePointer(to: &sin6) {
			try $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
				try body($0)
			}
		}
	}
}

internal extension in_addr {
	init(address: UInt32) {
		self.init()
		self.s_addr = address.bigEndian
	}
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
private func getaddrinfo_numeric(_ string: String, family: Int32 = 0) -> NWEndpoint.Host? {
	// Determine if this string has an interface scope specified "127.0.0.1%lo0" or "fe80::1%lo0"
	var string = string
	var interface : NWInterface? = nil
	if let range = string.range(of: "%", options: String.CompareOptions.backwards) {
		interface = NWInterface(String(string[range.upperBound...]))
		if interface != nil {
			string.removeSubrange(range.lowerBound...)
		}
	}

	// call getaddrinfo
	var hints = addrinfo(ai_flags: AI_NUMERICHOST, ai_family: family, ai_socktype: SOCK_STREAM, ai_protocol: 0,
						 ai_addrlen: 0, ai_canonname: nil, ai_addr: nil, ai_next: nil)
	var resolved : UnsafeMutablePointer<addrinfo>? = nil
	// After this point we must ensure we free addrinfo before we return
	guard getaddrinfo(string, nil, &hints, &resolved) == 0, let addrinfo = resolved else {
		return nil
	}

	var result: NWEndpoint.Host? = nil

	if let sa = addrinfo.pointee.ai_addr {
		if sa.pointee.sa_family == AF_INET {
			sa.withMemoryRebound(to: sockaddr_in.self, capacity: 1, { (sin) -> Void in
				result = NWEndpoint.Host.ipv4(IPv4Address(sin.pointee.sin_addr, interface))
			})
		} else if sa.pointee.sa_family == AF_INET6 {
			sa.withMemoryRebound(to: sockaddr_in6.self, capacity: 1, { (sin6) -> Void in
				if sin6.pointee.sin6_scope_id != 0 {
					interface = NWInterface(Int(sin6.pointee.sin6_scope_id))
				}
				let ipv6 = IPv6Address(sin6.pointee.sin6_addr, interface);
				if ipv6.isIPv4Mapped && family == AF_UNSPEC, let ipv4 = ipv6.asIPv4 {
					// Treat IPv4 mapped as IPv4
					result = NWEndpoint.Host.ipv4(ipv4)
				} else {
					result = NWEndpoint.Host.ipv6(ipv6)
				}
			})
		}
	}
	freeaddrinfo(addrinfo)
	return result
}

private func getnameinfo_numeric(address: UnsafeRawPointer) -> String {
	let sa = address.assumingMemoryBound(to: sockaddr.self)
	var result : String? = nil
	let maxLen = socklen_t(100)
	let storage = UnsafeMutablePointer<Int8>.allocate(capacity: Int(maxLen))
	if getnameinfo(sa, socklen_t(sa.pointee.sa_len), storage, maxLen, nil, 0, NI_NUMERICHOST) == 0 {
		result = String(cString: storage)
	}
	storage.deallocate()
	return result ?? "?"
}

/// An IP address
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public protocol IPAddress {

	/// Fetch the raw address as data
	var rawValue: Data { get }

	/// Create an IP address from data. The length of the data must
	/// match the expected length of addresses in the address family
	/// (four bytes for IPv4, and sixteen bytes for IPv6)
	init?(_ rawValue: Data, _ interface: NWInterface?)

	/// Create an IP address from an address literal string.
	/// If the string contains '%' to indicate an interface, the interface will be
	/// associated with the address, such as "::1%lo0" being associated with the loopback
	/// interface.
	/// This function does not perform host name to address resolution. This is the same as calling getaddrinfo
	/// and using AI_NUMERICHOST.
	init?(_ string: String)

	/// The interface the address is scoped to, if any.
	var interface: NWInterface? { get }

	/// Indicates if this address is loopback
	var isLoopback : Bool { get }

	/// Indicates if this address is link-local
	var isLinkLocal : Bool { get }

	/// Indicates if this address is multicast
	var isMulticast : Bool { get }
}

/// IPv4Address
/// Base type to hold an IPv4 address and convert between strings and raw bytes.
/// Note that an IPv4 address may be scoped to an interface.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public struct IPv4Address: IPAddress, Hashable, CustomDebugStringConvertible {

	/// The IPv4 any address used for listening
	public static let any = IPv4Address(in_addr(address: INADDR_ANY), nil)

	/// The IPv4 broadcast address used to broadcast to all hosts
	public static let broadcast = IPv4Address(in_addr(address: INADDR_BROADCAST), nil)

	/// The IPv4 loopback address
	public static let loopback = IPv4Address(in_addr(address: INADDR_LOOPBACK), nil)

	/// The IPv4 all hosts multicast group
	public static let allHostsGroup = IPv4Address(in_addr(address: INADDR_ALLHOSTS_GROUP), nil)

	/// The IPv4 all routers multicast group
	public static let allRoutersGroup = IPv4Address(in_addr(address: INADDR_ALLRTRS_GROUP), nil)

	/// The IPv4 all reports multicast group for ICMPv3 membership reports
	public static let allReportsGroup = IPv4Address(in_addr(address: INADDR_ALLRPTS_GROUP), nil)

	/// The IPv4 multicast DNS group. (Note: Use the dns_sd APIs instead of creating your own responder/resolver)
	public static let mdnsGroup = IPv4Address(in_addr(address: INADDR_ALLMDNS_GROUP), nil)

	/// Indicates if this IPv4 address is loopback (127.0.0.1)
	public var isLoopback : Bool {
		return self == IPv4Address.loopback
	}

	/// Indicates if this IPv4 address is link-local
	public var isLinkLocal : Bool {
		let linkLocalMask: UInt32 = IN_CLASSB_NET
		let linkLocalCompare: UInt32 = IN_LINKLOCALNETNUM
		return (self.address.s_addr & linkLocalMask.bigEndian) == linkLocalCompare.bigEndian
	}

	/// Indicates if this IPv4 address is multicast
	public var isMulticast : Bool {
		let multicastMask: UInt32 = IN_CLASSD_NET
		let multicastCompare: UInt32 = INADDR_UNSPEC_GROUP
		return (self.address.s_addr & multicastMask.bigEndian) == multicastCompare.bigEndian
	}

	/// Fetch the raw address (four bytes)
	public var rawValue: Data {
		var temporary = self.address
		return withUnsafeBytes(of: &temporary) { (bytes) -> Data in
			Data(bytes)
		}
	}

	internal init(_ address: in_addr, _ interface: NWInterface?) {
		self.address = address
		self.interface = interface
	}

	/// Create an IPv4 address from a 4-byte data. Optionally specify an interface.
	///
	/// - Parameter rawValue: The raw bytes of the IPv4 address, must be exactly 4 bytes or init will fail.
	/// - Parameter interface: An optional network interface to scope the address to. Defaults to nil.
	/// - Returns: An IPv4Address or nil if the Data parameter did not contain an IPv4 address.
	public init?(_ rawValue: Data, _ interface: NWInterface? = nil) {
		if rawValue.count != MemoryLayout<in_addr>.size {
			return nil
		}
		let v4 = rawValue.withUnsafeBytes { (ptr: UnsafePointer<in_addr>) -> in_addr in
			return ptr.pointee
		}
		self.init(v4, interface)
	}

	/// Create an IPv4 address from an address literal string.
	///
	/// This function does not perform host name to address resolution. This is the same as calling getaddrinfo
	/// and using AI_NUMERICHOST.
	///
	/// - Parameter string: An IPv4 address literal string such as "127.0.0.1", "169.254.8.8%en0".
	/// - Returns: An IPv4Address or nil if the string parameter did not
	/// contain an IPv4 address literal.
	public init?(_ string: String) {
		guard let result = getaddrinfo_numeric(string, family: AF_INET) else {
			return nil
		}
		guard case .ipv4(let address) = result else {
			return nil
		}
		self = address
	}

	fileprivate let address : in_addr

	/// The interface the address is scoped to, if any.
	public let interface: NWInterface?

	// Hashable
	public static func == (lhs: IPv4Address, rhs: IPv4Address) -> Bool {
		return lhs.address.s_addr == rhs.address.s_addr && lhs.interface == rhs.interface
	}

	public func hash(into hasher: inout Hasher) {
		hasher.combine(self.address.s_addr)
		hasher.combine(self.interface)
	}

	// CustomDebugStringConvertible
	public var debugDescription: String {
		var sin = sockaddr_in(self.address, 0)
		let addressString = getnameinfo_numeric(address: &sin)
		if let interface = self.interface {
			return String("\(addressString)%\(interface)")
		} else {
			return addressString
		}
	}
}

/// IPv6Address
/// Base type to hold an IPv6 address and convert between strings and raw bytes.
/// Note that an IPv6 address may be scoped to an interface.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public struct IPv6Address: IPAddress, Hashable, CustomDebugStringConvertible {

	/// IPv6 any address
	public static let any = IPv6Address(in6addr_any, nil)

	/// IPv6 broadcast address
	public static let broadcast = IPv6Address(in6addr_any, nil)

	/// IPv6 loopback address
	public static let loopback = IPv6Address(in6addr_loopback, nil)

	/// IPv6 all node local nodes multicast
	public static let nodeLocalNodes = IPv6Address(in6addr_nodelocal_allnodes, nil)

	/// IPv6 all link local nodes multicast
	public static let linkLocalNodes = IPv6Address(in6addr_linklocal_allnodes, nil)

	/// IPv6 all link local routers multicast
	public static let linkLocalRouters = IPv6Address(in6addr_linklocal_allrouters, nil)

	public enum Scope: UInt8 {
		case nodeLocal = 1
		case linkLocal = 2
		case siteLocal = 5
		case organizationLocal = 8
		case global = 0x0e
	}

	/// Is the Any address "::0"
	public var isAny : Bool {
		return self.address.__u6_addr.__u6_addr32.0 == 0 &&
			self.address.__u6_addr.__u6_addr32.1 == 0 &&
			self.address.__u6_addr.__u6_addr32.2 == 0 &&
			self.address.__u6_addr.__u6_addr32.3 == 0
	}

	/// Is the looback address "::1"
	public var isLoopback : Bool {
		return self.address.__u6_addr.__u6_addr32.0 == 0 &&
			self.address.__u6_addr.__u6_addr32.1 == 0 &&
			self.address.__u6_addr.__u6_addr32.2 == 0 &&
			self.address.__u6_addr.__u6_addr32.3 != 0 &&
			self.address.__u6_addr.__u6_addr32.3 == UInt32(1).bigEndian
	}

	/// Is an IPv4 compatible address
	public var isIPv4Compatabile : Bool {
		return self.address.__u6_addr.__u6_addr32.0 == 0 &&
			self.address.__u6_addr.__u6_addr32.1 == 0 &&
			self.address.__u6_addr.__u6_addr32.2 == 0 &&
			self.address.__u6_addr.__u6_addr32.3 != 0 &&
			self.address.__u6_addr.__u6_addr32.3 != UInt32(1).bigEndian
	}

	/// Is an IPv4 mapped address such as "::ffff:1.2.3.4"
	public var isIPv4Mapped : Bool {
		return self.address.__u6_addr.__u6_addr32.0 == 0 &&
			self.address.__u6_addr.__u6_addr32.1 == 0 &&
			self.address.__u6_addr.__u6_addr32.2 == UInt32(0x0000ffff).bigEndian
	}

	/// For IPv6 addresses that are IPv4 mapped, returns the IPv4 address
	///
	/// - Returns: nil unless the IPv6 address was mapped or compatible, in which case the IPv4 address is
	/// returned.
	public var asIPv4 : IPv4Address? {
		guard self.isIPv4Mapped || self.isIPv4Compatabile else {
			return nil
		}
		return IPv4Address(in_addr(address: self.address.__u6_addr.__u6_addr32.3.bigEndian),
										   self.interface)
	}

	/// Is a 6to4 IPv6 address
	public var is6to4 : Bool {
		return self.address.__u6_addr.__u6_addr16.0 == UInt16(0x2002).bigEndian
	}

	/// Is a link-local address
	public var isLinkLocal : Bool {
		return self.address.__u6_addr.__u6_addr8.0 == UInt8(0xfe) &&
			(self.address.__u6_addr.__u6_addr8.1 & 0xc0) == 0x80
	}

	/// Is multicast
	public var isMulticast : Bool {
		return self.address.__u6_addr.__u6_addr8.0 == 0xff
	}

	/// Returns the multicast scope
	public var multicastScope : IPv6Address.Scope? {
		if (self.isMulticast) {
			return IPv6Address.Scope(rawValue: self.address.__u6_addr.__u6_addr8.1 & 0x0f)
		}
		return nil
	}

	internal init(_ ip6: in6_addr, _ interface: NWInterface?) {
		self.address = ip6
		self.interface = interface
	}

	/// Create an IPv6 from a raw 16 byte value and optional interface
	///
	/// - Parameter rawValue: A 16 byte IPv6 address
	/// - Parameter interface: An optional interface the address is scoped to. Defaults to nil.
	/// - Returns: nil unless the raw data contained an IPv6 address
	public init?(_ rawValue: Data, _ interface: NWInterface? = nil) {
		if rawValue.count != MemoryLayout<in6_addr>.size {
			return nil
		}
		let v6 = rawValue.withUnsafeBytes { (ptr: UnsafePointer<in6_addr>) -> in6_addr in
			return ptr.pointee
		}
		self.init(v6, interface)
	}

	/// Create an IPv6 address from a string literal such as "fe80::1%lo0" or "2001:DB8::5"
	///
	/// This function does not perform hostname resolution. This is similar to calling getaddrinfo with
	/// AI_NUMERICHOST.
	///
	/// - Parameter string: An IPv6 address literal string.
	/// - Returns: nil unless the string contained an IPv6 literal
	public init?(_ string: String) {
		guard let result = getaddrinfo_numeric(string, family: AF_INET6) else {
			return nil
		}
		guard case .ipv6(let address) = result else {
			return nil
		}
		self = address
	}

	fileprivate let address: in6_addr

	/// The interface the address is scoped to, if any.
	public let interface: NWInterface?

	/// Fetch the raw address (sixteen bytes)
	public var rawValue: Data {
		var temporary = self.address
		return withUnsafeBytes(of: &temporary) { (bytes) -> Data in
			Data(bytes)
		}
	}

	// Hashable
	public static func ==(lhs: IPv6Address, rhs: IPv6Address) -> Bool {
		return lhs.address.__u6_addr.__u6_addr32.0 == rhs.address.__u6_addr.__u6_addr32.0 &&
			lhs.address.__u6_addr.__u6_addr32.1 == rhs.address.__u6_addr.__u6_addr32.1 &&
			lhs.address.__u6_addr.__u6_addr32.2 == rhs.address.__u6_addr.__u6_addr32.2 &&
			lhs.address.__u6_addr.__u6_addr32.3 == rhs.address.__u6_addr.__u6_addr32.3 &&
			lhs.interface == rhs.interface
	}

	public func hash(into hasher: inout Hasher) {
		hasher.combine(self.address.__u6_addr.__u6_addr32.0)
		hasher.combine(self.address.__u6_addr.__u6_addr32.1)
		hasher.combine(self.address.__u6_addr.__u6_addr32.2)
		hasher.combine(self.address.__u6_addr.__u6_addr32.3)
		hasher.combine(self.interface)
	}

	// CustomDebugStringConvertible
	public var debugDescription: String {
		var sin6 = sockaddr_in6(self.address, 0)
		let addressString = getnameinfo_numeric(address: &sin6)
		if let interface = self.interface {
			return String("\(addressString)%\(interface)")
		} else {
			return addressString
		}
	}
}

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
/// NWEndpoint represents something that can be connected to.
public enum NWEndpoint: Hashable, CustomDebugStringConvertible {
	// Depends on non-exhaustive enum support for forward compatibility - in the event we need to add
	// a new case in the future
	// https://github.com/apple/swift-evolution/blob/master/proposals/0192-non-exhaustive-enums.md

	/// A host port endpoint represents an endpoint defined by the host and port.
	case hostPort(host: NWEndpoint.Host, port: NWEndpoint.Port)

	/// A service endpoint represents a Bonjour service
	case service(name: String, type: String, domain: String, interface: NWInterface?)

	/// A unix endpoint represents a path that supports connections using AF_UNIX domain sockets.
	case unix(path: String)

	/// A Host is a name or address
	public enum Host: Hashable, CustomDebugStringConvertible, ExpressibleByStringLiteral {
		public typealias StringLiteralType = String

		public func hash(into hasher: inout Hasher) {
			switch self {
			case .name(let hostName, let hostInterface):
				hasher.combine(hostInterface)
				hasher.combine(hostName)
			case .ipv4(let v4Address):
				hasher.combine(v4Address)
			case .ipv6(let v6Address):
				hasher.combine(v6Address)
			}
		}

		/// A host specified as a name and optional interface scope
		case name(String, NWInterface?)

		/// A host specified as an IPv4 address
		case ipv4(IPv4Address)

		/// A host specified an an IPv6 address
		case ipv6(IPv6Address)

		public init(stringLiteral: StringLiteralType) {
			self.init(stringLiteral)
		}

		/// Create a host from a string.
		///
		/// This is the preferred way to create a host. If the string is an IPv4 address literal ("198.51.100.2"), an
		/// IPv4 host will be created. If the string is an IPv6 address literal ("2001:DB8::2", "fe80::1%lo", etc), an IPv6
		/// host will be created. If the string is an IPv4 mapped IPv6 address literal ("::ffff:198.51.100.2") an IPv4
		/// host will be created. Otherwise, a named host will be created.
		///
		/// - Parameter string: An IPv4 address literal, an IPv6 address literal, or a hostname.
		/// - Returns: A Host object
		public init(_ string: String) {
			if let result = getaddrinfo_numeric(string) {
				self = result
			} else {
				if let range = string.range(of: "%", options: String.CompareOptions.backwards),
					let interface = NWInterface(String(string[range.upperBound...])){
					self = .name(String(string[..<range.lowerBound]), interface)
				} else {
					self = .name(string, nil)
				}
			}
		}

		/// Returns the interface the host is scoped to if any
		public var interface : NWInterface? {
			switch self {
			case .ipv4(let ip4):
				return ip4.interface
			case .ipv6(let ip6):
				return ip6.interface
			case .name(_, let interface):
				return interface
			}
		}

		public var debugDescription: String {
			switch self {
			case .ipv4(let ip4):
				return ip4.debugDescription
			case .ipv6(let ip6):
				return ip6.debugDescription
			case .name(let name, let interface):
				if let interface = interface {
					return String("\(name)%\(interface)")
				} else {
					return name
				}
			}
		}
	}

	/// A network port (TCP or UDP)
	public struct Port : Hashable, CustomDebugStringConvertible, ExpressibleByIntegerLiteral, RawRepresentable {
		public typealias IntegerLiteralType = UInt16

		fileprivate let port: IntegerLiteralType

		public static let any : NWEndpoint.Port = 0
		public static let ssh : NWEndpoint.Port = 22
		public static let smtp : NWEndpoint.Port = 25
		public static let http : NWEndpoint.Port = 80
		public static let pop : NWEndpoint.Port = 110
		public static let imap : NWEndpoint.Port = 143
		public static let https : NWEndpoint.Port = 443
		public static let imaps : NWEndpoint.Port = 993
		public static let socks : NWEndpoint.Port = 1080

		public var rawValue: UInt16 {
			return self.port
		}

		/// Create a port from a string.
		///
		/// Supports common service names such as "http" as well as number strings such as "80".
        ///
        /// - Parameter service: A service string such as "http" or a number string such as "80"
        /// - Returns: A port if the string can be converted to a port, nil otherwise.
		public init?(_ service: String) {
			var hints = addrinfo(ai_flags: AI_DEFAULT, ai_family: AF_INET6, ai_socktype: SOCK_STREAM, ai_protocol: 0,
								 ai_addrlen: 0, ai_canonname: nil, ai_addr: nil, ai_next: nil)
			var resolved : UnsafeMutablePointer<addrinfo>? = nil
			if getaddrinfo(nil, service, &hints, &resolved) != 0 {
				return nil
			}

			// Check that it didn't return NULL.
			guard let addrinfo = resolved else {
				return nil
			}

			// After this point we must ensure we free addrinfo before we return
			guard let sa = addrinfo.pointee.ai_addr, sa.pointee.sa_family == AF_INET6 else {
				freeaddrinfo(addrinfo)
				return nil
			}

			self.port = sa.withMemoryRebound(to: sockaddr_in6.self, capacity: 1) {sin6 in
				return sin6.pointee.sin6_port.bigEndian
			}

			freeaddrinfo(addrinfo)
		}

		public init(integerLiteral value: IntegerLiteralType) {
			self.port = value
		}

		public init?(rawValue: UInt16) {
			self.port = rawValue
		}

		internal init(_ value: UInt16) {
			self.init(integerLiteral: value)
		}

		public var debugDescription: String {
			return String(port)
		}
	}

	/// Returns the interface the endpoint is scoped to if any
	public var interface : NWInterface? {
		switch self {
		case .hostPort(host: let host, port: _):
			return host.interface
		case .service(name: _, type: _, domain: _, interface: let interface):
			return interface
		case .unix(_):
			return nil
		}
	}

	internal init?(_ nw: nw_endpoint_t?) {
		guard let nw = nw else {
			return nil
		}
		var interface: NWInterface? = nil
		if let nwinterface = nw_endpoint_copy_interface(nw) {
			interface = NWInterface(nwinterface)
		}
		if nw_endpoint_get_type(nw) == Network.nw_endpoint_type_host {
			let host = NWEndpoint.Host.name(String(cString: nw_endpoint_get_hostname(nw)), interface)
			self = .hostPort(host: host, port: NWEndpoint.Port(nw_endpoint_get_port(nw)))
		} else if nw_endpoint_get_type(nw) == Network.nw_endpoint_type_address {
			let port = NWEndpoint.Port(nw_endpoint_get_port(nw))
			let address = nw_endpoint_get_address(nw)
			if address.pointee.sa_family == AF_INET && address.pointee.sa_len == MemoryLayout<sockaddr_in>.size {
                let host = address.withMemoryRebound(to: sockaddr_in.self, capacity: 1) {
                    (sin: UnsafePointer<sockaddr_in>) -> NWEndpoint.Host in
					return NWEndpoint.Host.ipv4(IPv4Address(sin.pointee.sin_addr, interface))
				}
				self = .hostPort(host: host, port: port)
			} else if address.pointee.sa_family == AF_INET6 &&
				address.pointee.sa_len == MemoryLayout<sockaddr_in6>.size {
				let host = address.withMemoryRebound(to: sockaddr_in6.self, capacity: 1) {
                    (sin6) -> NWEndpoint.Host in
					if interface == nil && sin6.pointee.sin6_scope_id != 0 {
						interface = NWInterface(Int(sin6.pointee.sin6_scope_id))
					}
					return NWEndpoint.Host.ipv6(IPv6Address(sin6.pointee.sin6_addr,
                                                                                      interface))
				}
				self = .hostPort(host: host, port: port)
			} else if address.pointee.sa_family == AF_UNIX {
				// sockaddr_un is very difficult to deal with in swift. Fortunately, nw_endpoint_copy_address_string
				// already does exactly what we need.
				let path = nw_endpoint_copy_address_string(nw)
				self = .unix(path: String(cString: path))
				path.deallocate()
			} else {
				return nil
			}
		} else if nw_endpoint_get_type(nw) == Network.nw_endpoint_type_bonjour_service {
			self = .service(name: String(cString: nw_endpoint_get_bonjour_service_name(nw)),
							type: String(cString: nw_endpoint_get_bonjour_service_type(nw)),
							domain: String(cString: nw_endpoint_get_bonjour_service_domain(nw)),
							interface: interface)
		} else {
			return nil
		}
	}

	public func hash(into hasher: inout Hasher) {
		switch self {
		case .hostPort(host: let host, port: let port):
			hasher.combine(host)
			hasher.combine(port)
		case .service(name: let name, type: let type, domain: let domain, interface: let interface):
			hasher.combine(name)
			hasher.combine(type)
			hasher.combine(domain)
			hasher.combine(interface)
		case .unix(let path):
			hasher.combine(path)
		}
	}

	public var debugDescription: String {
		switch self {
		case .hostPort(host: let host, port: let port):
			var separator = ":"
			if case .ipv6 = host {
				separator = "."
			}
			return String("\(host)\(separator)\(port)")
		case .service(name: let name, type: let type, domain: let domain, interface: let interface):
			if let interface = interface {
				return String("\(name).\(type)\(domain)%\(interface)")
			}
			return String("\(name).\(type)\(domain)")
		case .unix(let path):
			return path
		}
	}

	internal var nw: nw_endpoint_t? {
		var endpoint: nw_endpoint_t? = nil
		var interface: NWInterface? = nil
		switch self {
		case .hostPort(host: let host, port: let port):
			switch host {
			case .ipv4(let ipv4):
				let sin = sockaddr_in(ipv4.address, port.port.bigEndian)
				endpoint = sin.withSockAddr { (sa) -> nw_endpoint_t in
					nw_endpoint_create_address(sa)
				}
				interface = ipv4.interface
			case .ipv6(let ipv6):
				let sin6 = sockaddr_in6(ipv6.address, port.port.bigEndian)
				endpoint = sin6.withSockAddr { (sa) -> nw_endpoint_t? in
					nw_endpoint_create_address(sa)
				}
				interface = ipv6.interface
			case .name(let host, let hostInterface):
				endpoint = nw_endpoint_create_host(host, port.debugDescription)
				interface = hostInterface
			}
		case .service(name: let name, type: let type, domain: let domain, interface: let bonjourInterface):
			endpoint = nw_endpoint_create_bonjour_service(name, type, domain)
			interface = bonjourInterface
		case .unix(let path):
			endpoint = nw_endpoint_create_unix(path)
		}
		if interface != nil && endpoint != nil {
			nw_endpoint_set_interface(endpoint!, interface!.nw)
		}
		return endpoint
	}
}
