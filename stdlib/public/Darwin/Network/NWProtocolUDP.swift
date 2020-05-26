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

// NOTE: older overlays had Network.NWProtocolUDP as the ObjC name.
// The two must coexist, so it was renamed. The old name must not be
// used in the new runtime. _TtC7Network14_NWProtocolUDP is the
// mangled name for Network._NWProtocolUDP.
@_objcRuntimeName(_TtC7Network14_NWProtocolUDP)
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public class NWProtocolUDP : NWProtocol {
	public static let definition: NWProtocolDefinition = {
		NWProtocolDefinition(nw_protocol_copy_udp_definition(), "udp")
	}()

  // Set the ObjC name of this class to be nested in the customized ObjC
  // name of NWProtocolUDP.
  @_objcRuntimeName(_TtCC7Network14_NWProtocolUDP7Options)
	public class Options : NWProtocolOptions {

		private var _preferNoChecksum: Bool = false

		/// Configure UDP to skip computing checksums when sending.
		/// This will only take effect when running over IPv4 (UDP_NOCKSUM).
		public var preferNoChecksum: Bool {
			set {
				self._preferNoChecksum = newValue
				nw_udp_options_set_prefer_no_checksum(self.nw, newValue)
			}
			get {
				return self._preferNoChecksum
			}
		}

		/// Create UDP options to set in an NWParameters.ProtocolStack
		public init() {
			super.init(nw_udp_create_options())
		}

		override internal init(_ nw: nw_protocol_options_t) {
			super.init(nw)
		}
	}

  // Set the ObjC name of this class to be nested in the customized ObjC
  // name of NWProtocolUDP.
  @_objcRuntimeName(_TtCC7Network14_NWProtocolUDP8Metadata)
  public class Metadata: NWProtocolMetadata {
		override internal init(_ nw: nw_protocol_metadata_t) {
			super.init(nw)
		}

		/// Create an empty UDP metadata to send with ContentContext
		public init() {
			super.init(nw_udp_create_metadata())
		}
	}
}
