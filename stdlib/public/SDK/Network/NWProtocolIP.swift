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

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public class NWProtocolIP : NWProtocol {
	public static let definition: NWProtocolDefinition = {
		NWProtocolDefinition(nw_protocol_copy_ip_definition(), "ip")
	}()

	public class Options : NWProtocolOptions {
		public enum Version {
			/// Allow any IP version
			case any
			/// Use only IP version 4 (IPv4)
			case v4
			/// Use only IP version 6 (IPv6)
			case v6

			internal var nw: nw_ip_version_t {
				switch self {
				case .v4:
					return Network.nw_ip_version_4
				case .v6:
					return Network.nw_ip_version_6
				default:
					return Network.nw_ip_version_any
				}
			}

			internal init(_ nw: nw_ip_version_t) {
				switch nw {
				case Network.nw_ip_version_4:
					self = .v4
				case Network.nw_ip_version_6:
					self = .v6
				default:
					self = .any
				}
			}
		}

		private var _version: Version = .any

		/// Specify a single version of the Internet NWProtocol to allow.
		/// Setting this value will constrain which address endpoints can
		/// be used, and will filter DNS results during connection establishment.
		public var version: Version {
			set {
				self._version = newValue
				nw_ip_options_set_version(self.nw, newValue.nw)
			}
			get {
				return self._version
			}
		}

		private var _hopLimit: UInt8 = 0

		/// Configure the IP hop limit, equivalent to IP_TTL for IPv4
		/// and IPV6_HOPLIMIT for IPv6.
		public var hopLimit: UInt8 {
			set {
				self._hopLimit = newValue
				nw_ip_options_set_hop_limit(self.nw, newValue)
			}
			get {
				return self._hopLimit
			}
		}

		private var _useMinimumMTU: Bool = false

		/// Configure IP to use the minimum MTU value, which
		/// is 1280 bytes for IPv6 (IPV6_USE_MIN_MTU). This value has
		/// no effect for IPv4.
		public var useMinimumMTU: Bool {
			set {
				self._useMinimumMTU = newValue
				nw_ip_options_set_use_minimum_mtu(self.nw, newValue)
			}
			get {
				return self._useMinimumMTU
			}
		}

		private var _disableFragmentation: Bool = false

		/// Configure IP to disable fragmentation on outgoing
		/// packets (IPV6_DONTFRAG). This value has no effect
		/// for IPv4.
		public var disableFragmentation: Bool {
			set {
				self._disableFragmentation = newValue
				nw_ip_options_set_disable_fragmentation(self.nw, newValue)
			}
			get {
				return self._disableFragmentation
			}
		}

        private var _shouldCalculateReceiveTime: Bool = false

        /// Configure IP to calculate receive time for inbound
        /// packets.
        public var shouldCalculateReceiveTime: Bool {
            set {
                self._shouldCalculateReceiveTime = newValue
                nw_ip_options_set_calculate_receive_time(self.nw, newValue)
            }
            get {
                return self._shouldCalculateReceiveTime
            }
        }

		override internal init(_ nw: nw_protocol_options_t) {
			super.init(nw)
		}
	}

	/// Values for Explicit Congestion Notification flags
	public enum ECN {
		/// Non ECN-Capable Transport
		case nonECT
		/// ECN Capable Transport (0)
		case ect0
		/// ECN Capable Transport (1)
		case ect1
		/// Congestion Experienced
		case ce

		fileprivate init(_ nw: nw_ip_ecn_flag_t) {
			switch nw {
			case Network.nw_ip_ecn_flag_non_ect:
				self = .nonECT
			case Network.nw_ip_ecn_flag_ect_0:
				self = .ect0
			case Network.nw_ip_ecn_flag_ect_1:
				self = .ect1
			case Network.nw_ip_ecn_flag_ce:
				self = .ce
			default:
				self = .nonECT
			}
		}

		fileprivate var nw : nw_ip_ecn_flag_t {
			switch self {
			case .nonECT:
				return Network.nw_ip_ecn_flag_non_ect
			case .ect0:
				return Network.nw_ip_ecn_flag_ect_0
			case .ect1:
				return Network.nw_ip_ecn_flag_ect_1
			case .ce:
				return Network.nw_ip_ecn_flag_ce
			}
		}
	}

	/// IP Metadata can be sent or received as part of ContentContext
	public class Metadata: NWProtocolMetadata {
		/// Set ECN flags to be sent on a packet, or get ECN flags
		/// received on a packet. These flags will not take effect
		/// for protocols such as TCP that deliver data without accounting
		/// for packet boundaries.
		public var ecn: ECN {
			set {
				nw_ip_metadata_set_ecn_flag(nw, newValue.nw)
			}
			get {
				return ECN(nw_ip_metadata_get_ecn_flag(nw))
			}
		}

		/// Set the network service class to be sent on a packet. The per-packet
		/// service class will not take effect for protocols such as TCP that deliver
		/// data without accounting for packet boundaries. If you need to set
		/// service class for TCP, use the serviceClass property of NWParameters.
        public var serviceClass : NWParameters.ServiceClass {
            set {
                nw_ip_metadata_set_service_class(nw, newValue.nw)
            }
            get {
                return NWParameters.ServiceClass(nw_ip_metadata_get_service_class(nw))
            }
        }

        /// The time at which a packet was received, in nanoseconds.
        /// Equivalent to timestamps returned by CLOCK_MONOTONIC_RAW.
        public var receiveTime: UInt64 {
            get {
                return nw_ip_metadata_get_receive_time(nw)
            }
        }

		override internal init(_ nw: nw_protocol_metadata_t) {
			super.init(nw)
		}

		/// Create an empty IP metadata to send with ContentContext
		public init() {
			super.init(nw_ip_create_metadata())
		}
	}
}
