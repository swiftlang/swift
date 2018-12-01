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

/// An NWParameters object contains the parameters necessary to create
/// a network connection or listener. NWParameters include any preferences for
/// network paths (such as required, prohibited, and preferred networks, and local
/// endpoint requirements); preferences for data transfer and quality of service;
/// and the protocols to be used for a connection along with any protocol-specific
/// options.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public final class NWParameters : CustomDebugStringConvertible {
	public var debugDescription: String {
		return String("\(self.nw)")
	}

	internal let nw : nw_parameters_t

	/// Creates a parameters object that is configured for TLS and TCP. The caller can use
	/// the default configuration for TLS and TCP, or set specific options for each protocol,
	/// or disable TLS.
    ///
    /// - Parameter tls: TLS options or nil for no TLS
    /// - Parameter tcp: TCP options. Defaults to NWProtocolTCP.Options() with no options overridden.
    /// - Returns: NWParameters object that can be used for creating a connection or listener
	public convenience init(tls: NWProtocolTLS.Options?, tcp: NWProtocolTCP.Options = NWProtocolTCP.Options()) {
		self.init()
		let protocolStack = self.defaultProtocolStack
		protocolStack.transportProtocol = tcp
		if let tls = tls {
			protocolStack.applicationProtocols = [tls]
		} else {
			protocolStack.applicationProtocols = []
		}
	}

	/// Creates a parameters object that is configured for DTLS and UDP. The caller can use
	/// the default configuration for DTLS and UDP, or set specific options for each protocol,
	/// or disable TLS.
    ///
    /// - Parameter dtls: DTLS options or nil for no DTLS
    /// - Parameter udp: UDP options. Defaults to NWProtocolUDP.Options() with no options overridden.
    /// - Returns: NWParameters object that can be used for create a connection or listener
	public convenience init(dtls: NWProtocolTLS.Options?, udp: NWProtocolUDP.Options = NWProtocolUDP.Options()) {
		self.init()
		let protocolStack = self.defaultProtocolStack
		protocolStack.transportProtocol = udp
		if let dtls = dtls {
			protocolStack.applicationProtocols = [dtls]
		} else {
			protocolStack.applicationProtocols = []
		}
	}

	/// Creates a generic NWParameters object. Note that in order to use parameters
	/// with a NWConnection or a NetworkListener, the parameters must have protocols
	/// added into the defaultProtocolStack. Clients using standard protocol
	/// configurations should use init(tls:tcp:) or init(dtls:udp:).
	public init() {
		self.nw = nw_parameters_create()
	}

	private init(nw: nw_parameters_t) {
		self.nw = nw
	}

	/// Default set of parameters for TLS over TCP
	/// This is equivalent to calling init(tls:NWProtocolTLS.Options(), tcp:NWProtocolTCP.Options())
	public class var tls: NWParameters {
		return NWParameters(tls:NWProtocolTLS.Options())
	}

	/// Default set of parameters for DTLS over UDP
	/// This is equivalent to calling init(dtls:NWProtocolTLS.Options(), udp:NWProtocolUDP.Options())
	public class var dtls: NWParameters {
		return NWParameters(dtls:NWProtocolTLS.Options())
	}

	/// Default set of parameters for TCP
	/// This is equivalent to calling init(tls:nil, tcp:NWProtocolTCP.Options())
	public class var tcp: NWParameters {
		return NWParameters(tls:nil)
	}

	/// Default set of parameters for UDP
	/// This is equivalent to calling init(dtls:nil, udp:NWProtocolUDP.Options())
	public class var udp: NWParameters {
		return NWParameters(dtls:nil)
	}

	/// Require a connection to use a specific interface, or fail if not available
	public var requiredInterface: NWInterface? {
		set {
			if let interface = newValue {
				nw_parameters_require_interface(self.nw, interface.nw)
			} else {
				nw_parameters_require_interface(self.nw, nil)
			}
		}
		get {
			if let interface = nw_parameters_copy_required_interface(self.nw) {
				return NWInterface(interface)
			} else {
				return nil
			}
		}
	}

	/// Require a connection to use a specific interface type, or fail if not available
	public var requiredInterfaceType: NWInterface.InterfaceType {
		set {
			nw_parameters_set_required_interface_type(self.nw, newValue.nw)
		}
		get {
			return NWInterface.InterfaceType(nw_parameters_get_required_interface_type(self.nw))
		}
	}

	/// Define one or more interfaces that a connection will not be allowed to use
	public var prohibitedInterfaces: [NWInterface]? {
		set {
			nw_parameters_clear_prohibited_interfaces(self.nw)
			if let prohibitedInterfaces = newValue {
				for prohibitedInterface in prohibitedInterfaces {
					nw_parameters_prohibit_interface(self.nw, prohibitedInterface.nw)
				}
			}
		}
		get {
			var interfaces = [NWInterface]()
			nw_parameters_iterate_prohibited_interfaces(self.nw) { (interface) in
				interfaces.append(NWInterface(interface))
				return true
			}
			return interfaces
		}
	}

	/// Define one or more interface types that a connection will not be allowed to use
	public var prohibitedInterfaceTypes: [NWInterface.InterfaceType]? {
		set {
			nw_parameters_clear_prohibited_interface_types(self.nw)
			if let prohibitedInterfaceTypes = newValue {
				for prohibitedInterfaceType in prohibitedInterfaceTypes {
					nw_parameters_prohibit_interface_type(self.nw, prohibitedInterfaceType.nw)
				}
			}
		}
		get {
			var interfaceTypes = [NWInterface.InterfaceType]()
			nw_parameters_iterate_prohibited_interface_types(self.nw) { (interfaceType) in
				interfaceTypes.append(NWInterface.InterfaceType(interfaceType))
				return true
			}
			return interfaceTypes
		}
	}

	/// Disallow connection from using interfaces considered expensive
	public var prohibitExpensivePaths: Bool {
		set {
			nw_parameters_set_prohibit_expensive(self.nw, newValue)
		}
		get {
			return nw_parameters_get_prohibit_expensive(self.nw)
		}
	}

	/// If true, a direct connection will be attempted first even if proxies are configured. If the direct connection
	/// fails, connecting through the proxies will still be attempted.
	public var preferNoProxies: Bool {
		set {
			nw_parameters_set_prefer_no_proxy(self.nw, newValue)
		}
		get {
			return nw_parameters_get_prefer_no_proxy(self.nw)
		}
	}

	/// Force a specific local address to be used. This value is nil by
	/// default, in which case the system selects the most appropriate
	/// local address and selects a local port.
	public var requiredLocalEndpoint: NWEndpoint? {
		set {
			if let endpoint = newValue {
				nw_parameters_set_local_endpoint(self.nw, endpoint.nw)
			} else {
				nw_parameters_set_local_endpoint(self.nw, nil)
			}
		}
		get {
			if let endpoint = nw_parameters_copy_local_endpoint(self.nw) {
				return NWEndpoint(endpoint)
			} else {
				return nil
			}
		}
	}

	/// Allow multiple connections to use the same local address and port
	/// (SO_REUSEADDR and SO_REUSEPORT).
	public var allowLocalEndpointReuse: Bool {
		set {
			nw_parameters_set_reuse_local_address(self.nw, newValue)
		}
		get {
			return nw_parameters_get_reuse_local_address(self.nw)
		}
	}

	/// Cause an NWListener to only advertise services on the local link,
	/// and only accept connections from the local link.
	public var acceptLocalOnly: Bool {
		set {
			nw_parameters_set_local_only(self.nw, newValue)
		}
		get {
			return nw_parameters_get_local_only(self.nw)
		}
	}

    /// Allow the inclusion of peer-to-peer interfaces when
    /// listening or establishing outbound connections. This parameter
    /// will not take effect if a specific interface is required.
    /// This parameter is applicable when advertising a Bonjour service
    /// on a listener, or connecting to a Bonjour service.
    public var includePeerToPeer: Bool {
        set {
            nw_parameters_set_include_peer_to_peer(self.nw, newValue)
        }
        get {
            return nw_parameters_get_include_peer_to_peer(self.nw)
        }
    }

	/// The ServiceClass represents the network queuing priority to use
	/// for traffic generated by a NWConnection.
	public enum ServiceClass {
		/// Default priority traffic
		case bestEffort
		/// Bulk traffic, or traffic that can be de-prioritized behind foreground traffic
		case background
		/// Interactive video traffic
		case interactiveVideo
		/// Interactive voice traffic
		case interactiveVoice
        /// Responsive data
        case responsiveData
        /// Signaling
        case signaling

		internal var nw: nw_service_class_t {
			switch self {
			case .bestEffort:
				return Network.nw_service_class_best_effort
			case .background:
				return Network.nw_service_class_background
			case .interactiveVideo:
				return Network.nw_service_class_interactive_video
			case .interactiveVoice:
				return Network.nw_service_class_interactive_voice
            case .responsiveData:
                return Network.nw_service_class_responsive_data
            case .signaling:
                return Network.nw_service_class_signaling
			}
			}

		internal init(_ nw: nw_service_class_t) {
			switch nw {
			case Network.nw_service_class_best_effort:
				self = .bestEffort
			case Network.nw_service_class_background:
				self = .background
			case Network.nw_service_class_interactive_video:
				self = .interactiveVideo
			case Network.nw_service_class_interactive_voice:
				self = .interactiveVoice
			case Network.nw_service_class_responsive_data:
				self = .responsiveData
			case Network.nw_service_class_signaling:
				self = .signaling
			default:
				self = .bestEffort
			}
		}
	}
	public var serviceClass: NWParameters.ServiceClass {
		set {
			nw_parameters_set_service_class(self.nw, newValue.nw)
		}
		get {
			return NWParameters.ServiceClass(nw_parameters_get_service_class(self.nw))
		}
	}

	/// Multipath services represent the modes of multipath usage that are
	/// allowed for connections.
	public enum MultipathServiceType {
		/// No multipath transport will be attempted
		case disabled
		/// Only use the expensive interface when the when the primary one is not available
		case handover
		/// Use the expensive interface more aggressively to reduce latency
		case interactive
		/// Use all available interfaces to provide the highest throughput and lowest latency
		case aggregate

		internal var nw: nw_multipath_service_t {
			switch self {
			case .disabled:
				return Network.nw_multipath_service_disabled
			case .handover:
				return Network.nw_multipath_service_handover
			case .interactive:
				return Network.nw_multipath_service_interactive
			case .aggregate:
				return Network.nw_multipath_service_aggregate
			}
		}

		internal init(_ nw: nw_multipath_service_t) {
			switch nw {
			case Network.nw_multipath_service_disabled:
				self = .disabled
			case Network.nw_multipath_service_handover:
				self = .handover
			case Network.nw_multipath_service_interactive:
				self = .interactive
			case Network.nw_multipath_service_aggregate:
				self = .aggregate
			default:
				self = .disabled
			}
		}
	}
	public var multipathServiceType: NWParameters.MultipathServiceType {
		set {
			nw_parameters_set_multipath_service(self.nw, newValue.nw)
		}
		get {
			return NWParameters.MultipathServiceType(nw_parameters_get_multipath_service(self.nw))
		}
	}

	/// Use fast open for an outbound NWConnection, which may be done at any
	/// protocol level. Use of fast open requires that the caller send
	/// idempotent data on the connection before the connection may move
	/// into ready state. As a side effect, this may implicitly enable
	/// fast open for protocols in the stack, even if they did not have
	/// fast open explicitly enabled on them (such as the option to enable
	/// TCP Fast Open).
	public var allowFastOpen: Bool {
		set {
			nw_parameters_set_fast_open_enabled(self.nw, newValue)
		}
		get {
			return nw_parameters_get_fast_open_enabled(self.nw)
		}
	}

	/// Allow or prohibit the use of expired DNS answers during connection establishment.
	/// If allowed, a DNS answer that was previously returned may be re-used for new
	/// connections even after the answers are considered expired. A query for fresh answers
	/// will be sent in parallel, and the fresh answers will be used as alternate addresses
	/// in case the expired answers do not result in successful connections.
	/// By default, this value is .systemDefault, which allows the system to determine
	/// if it is appropriate to use expired answers.
	public enum ExpiredDNSBehavior {
		/// Let the system determine whether or not to allow expired DNS answers
		case systemDefault
		/// Explicitly allow the use of expired DNS answers
		case allow
		/// Explicitly prohibit the use of expired DNS answers
		case prohibit

		internal var nw: nw_parameters_expired_dns_behavior_t {
			switch self {
			case .systemDefault:
				return Network.nw_parameters_expired_dns_behavior_default
			case .allow:
				return Network.nw_parameters_expired_dns_behavior_allow
			case .prohibit:
				return Network.nw_parameters_expired_dns_behavior_prohibit
			}
		}

		internal init(_ nw: nw_parameters_expired_dns_behavior_t) {
			switch nw {
			case Network.nw_parameters_expired_dns_behavior_default:
				self = .systemDefault
			case Network.nw_parameters_expired_dns_behavior_allow:
				self = .allow
			case Network.nw_parameters_expired_dns_behavior_prohibit:
				self = .prohibit
			default:
				self = .systemDefault
			}
		}
	}
	public var expiredDNSBehavior: NWParameters.ExpiredDNSBehavior {
		set {
			nw_parameters_set_expired_dns_behavior(self.nw, newValue.nw)
		}
		get {
			return NWParameters.ExpiredDNSBehavior(nw_parameters_get_expired_dns_behavior(self.nw))
		}
	}

	/// A ProtocolStack contains a list of protocols to use for a connection.
	/// The members of the protocol stack are NWProtocolOptions objects, each
	/// defining which protocol to use within the stack along with any protocol-specific
	/// options. Each stack includes an array of application-level protocols, a single
	/// transport-level protocol, and an optional internet-level protocol. If the internet-
	/// level protocol is not specified, any available and applicable IP address family
	/// may be used.
	public class ProtocolStack {
		public var applicationProtocols: [NWProtocolOptions] {
			set {
				nw_protocol_stack_clear_application_protocols(self.nw)
				for applicationProtocol in newValue.reversed() {
					nw_protocol_stack_prepend_application_protocol(self.nw, applicationProtocol.nw)
				}
			}
			get {
				var applicationProtocols = [NWProtocolOptions]()
				nw_protocol_stack_iterate_application_protocols(self.nw) { (protocolOptions) in
					let applicationDefinition = nw_protocol_options_copy_definition(protocolOptions)
					if (nw_protocol_definition_is_equal(applicationDefinition, NWProtocolTLS.definition.nw)) {
						applicationProtocols.append(NWProtocolTLS.Options(protocolOptions))
					}
				}
				return applicationProtocols
			}
		}

		public var transportProtocol: NWProtocolOptions? {
			set {
				if let transport = newValue {
					nw_protocol_stack_set_transport_protocol(self.nw, transport.nw)
				}
			}
			get {
				if let transport = nw_protocol_stack_copy_transport_protocol(nw) {
					let transportDefinition = nw_protocol_options_copy_definition(transport)
					if (nw_protocol_definition_is_equal(transportDefinition, NWProtocolTCP.definition.nw)) {
						return NWProtocolTCP.Options(transport)
					} else if (nw_protocol_definition_is_equal(transportDefinition, NWProtocolUDP.definition.nw)) {
						return NWProtocolUDP.Options(transport)
					}
				}
				return nil
			}
		}

		public var internetProtocol: NWProtocolOptions? {
			set {
				// Not currently allowed
				return
			}
			get {
				if let ip = nw_protocol_stack_copy_internet_protocol(nw) {
					if (nw_protocol_definition_is_equal(nw_protocol_options_copy_definition(ip), NWProtocolIP.definition.nw)) {
						return NWProtocolIP.Options(ip)
					}
				}
				return nil
			}
		}

		internal let nw: nw_protocol_stack_t

		internal init(_ nw: nw_protocol_stack_t) {
			self.nw = nw
		}
	}

	/// Every NWParameters has a default protocol stack, although it may start out empty.
	public var defaultProtocolStack: NWParameters.ProtocolStack {
		get {
			return NWParameters.ProtocolStack(nw_parameters_copy_default_protocol_stack(self.nw))
		}
	}

	/// Perform a deep copy of parameters
	public func copy() -> NWParameters {
		return NWParameters(nw: nw_parameters_copy(self.nw))
	}
}
