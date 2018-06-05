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
public class NWProtocolTCP : NWProtocol {
	public static let definition: NWProtocolDefinition = {
		NWProtocolDefinition(nw_protocol_copy_tcp_definition(), "tcp")
	}()

	public class Options : NWProtocolOptions {

		private var _noDelay: Bool = false

		/// A boolean indicating that TCP should disable
		/// Nagle's algorithm (TCP_NODELAY).
		public var noDelay: Bool {
			set {
				self._noDelay = newValue
				nw_tcp_options_set_no_delay(self.nw, newValue)
			}
			get {
				return self._noDelay
			}
		}

		private var _noPush: Bool = false

		/// A boolean indicating that TCP should be set into
		/// no-push mode (TCP_NOPUSH).
		public var noPush: Bool {
			set {
				self._noPush = newValue
				nw_tcp_options_set_no_push(self.nw, newValue)
			}
			get {
				return self._noPush
			}
		}

		private var _noOptions: Bool = false

		/// A boolean indicating that TCP should be set into
		/// no-options mode (TCP_NOOPT).
		public var noOptions: Bool {
			set {
				self._noOptions = newValue
				nw_tcp_options_set_no_options(self.nw, newValue)
			}
			get {
				return self._noOptions
			}
		}

		private var _enableKeepalive: Bool = false

		/// A boolean indicating that TCP should send keepalives
		/// (SO_KEEPALIVE).
		public var enableKeepalive: Bool {
			set {
				self._enableKeepalive = newValue
				nw_tcp_options_set_enable_keepalive(self.nw, newValue)
			}
			get {
				return self._enableKeepalive
			}
		}

		private var _keepaliveCount: Int = 0

		/// The number of keepalive probes to send before terminating
		/// the connection (TCP_KEEPCNT).
		public var keepaliveCount : Int {
			set {
				self._keepaliveCount = newValue
				nw_tcp_options_set_keepalive_count(self.nw, UInt32(newValue))
			}
			get {
				return self._keepaliveCount
			}
		}

		private var _keepaliveIdle: Int = 0

		/// The number of seconds of idleness to wait before keepalive
		/// probes are sent by TCP (TCP_KEEPALIVE).
		public var keepaliveIdle : Int {
			set {
				self._keepaliveIdle = newValue
				nw_tcp_options_set_keepalive_idle_time(self.nw, UInt32(newValue))
			}
			get {
				return self._keepaliveIdle
			}
		}

		private var _keepaliveInterval: Int = 0

		/// The number of seconds of to wait before resending TCP
		/// keepalive probes (TCP_KEEPINTVL).
		public var keepaliveInterval : Int {
			set {
				self._keepaliveInterval = newValue
				nw_tcp_options_set_keepalive_interval(self.nw, UInt32(newValue))
			}
			get {
				return self._keepaliveInterval
			}
		}

		private var _maximumSegmentSize: Int = 0

		/// The maximum segment size in bytes (TCP_MAXSEG).
		public var maximumSegmentSize : Int {
			set {
				self._maximumSegmentSize = newValue
				nw_tcp_options_set_maximum_segment_size(self.nw, UInt32(newValue))
			}
			get {
				return self._maximumSegmentSize
			}
		}

		private var _connectionTimeout: Int = 0

		/// A timeout for TCP connection establishment, in seconds
		/// (TCP_CONNECTIONTIMEOUT).
		public var connectionTimeout : Int {
			set {
				self._connectionTimeout = newValue
				nw_tcp_options_set_connection_timeout(self.nw, UInt32(newValue))
			}
			get {
				return self._connectionTimeout
			}
		}

		private var _persistTimeout: Int = 0

		/// The TCP persist timeout, in seconds (PERSIST_TIMEOUT).
		/// See RFC 6429.
		public var persistTimeout : Int {
			set {
				self._persistTimeout = newValue
				nw_tcp_options_set_persist_timeout(self.nw, UInt32(newValue))
			}
			get {
				return self._persistTimeout
			}
		}

		private var _connectionDropTime: Int = 0

		/// A timeout for TCP retransmission attempts, in seconds
		/// (TCP_RXT_CONNDROPTIME).
		public var connectionDropTime : Int {
			set {
				self._connectionDropTime = newValue
				nw_tcp_options_set_retransmit_connection_drop_time(self.nw, UInt32(newValue))
			}
			get {
				return self._connectionDropTime
			}
		}

		private var _retransmitFinDrop: Bool = false

		/// A boolean to cause TCP to drop its connection after
		/// not receiving an ACK after a FIN (TCP_RXT_FINDROP).
		public var retransmitFinDrop: Bool {
			set {
				self._retransmitFinDrop = newValue
				nw_tcp_options_set_retransmit_fin_drop(self.nw, newValue)
			}
			get {
				return self._retransmitFinDrop
			}
		}

		private var _disableAckStretching: Bool = false

		/// A boolean to cause TCP to disable ACK stretching (TCP_SENDMOREACKS).
		public var disableAckStretching: Bool {
			set {
				self._disableAckStretching = newValue
				nw_tcp_options_set_disable_ack_stretching(self.nw, newValue)
			}
			get {
				return self._disableAckStretching
			}
		}

		private var _enableFastOpen: Bool = false

		/// Configure TCP to enable TCP Fast Open (TFO). This may take effect
		/// even when TCP is not the top-level protocol in the protocol stack.
		/// For example, if TLS is running over TCP, the Client Hello message
		/// may be sent as fast open data.
		///
		/// If TCP is the top-level protocol in the stack (the one the application
		/// directly interacts with), TFO will be disabled unless the application
		/// indicated that it will provide its own fast open data by calling
		/// NWParameters.allowFastOpen.
		public var enableFastOpen: Bool {
			set {
				self._enableFastOpen = newValue
				nw_tcp_options_set_enable_fast_open(self.nw, newValue)
			}
			get {
				return self._enableFastOpen
			}
		}

		private var _disableECN: Bool = false

		/// A boolean to disable ECN negotiation in TCP.
		public var disableECN: Bool {
			set {
				self._disableECN = newValue
				nw_tcp_options_set_disable_ecn(self.nw, newValue)
			}
			get {
				return self._disableECN
			}
		}

		/// Create TCP options to set in an NWParameters.ProtocolStack
		public init() {
			super.init(nw_tcp_create_options())
		}

		override internal init(_ nw: nw_protocol_options_t) {
			super.init(nw)
		}
	}

	/// Access TCP metadata using NWConnection.metadata(protocolDefinition: NWProtocolTCP.definition)
	/// or in received ContentContext
	public class Metadata: NWProtocolMetadata {
		override internal init(_ nw: nw_protocol_metadata_t) {
			super.init(nw)
		}

		/// Access the current number of bytes in TCP's receive buffer (SO_NREAD).
        public var availableReceiveBuffer: UInt32 {
            get {
                return nw_tcp_get_available_receive_buffer(self.nw)
            }
        }

		/// Access the current number of bytes in TCP's send buffer (SO_NWRITE).
        public var availableSendBuffer: UInt32 {
            get {
                return nw_tcp_get_available_send_buffer(self.nw)
            }
        }
	}
}
