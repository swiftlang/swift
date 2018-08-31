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
public class NWProtocolTLS : NWProtocol {
	public static let definition: NWProtocolDefinition = {
		NWProtocolDefinition(nw_protocol_copy_tls_definition(), "tls")
	}()

	public class Options : NWProtocolOptions {
		/// Access the sec_protocol_options_t for a given network protocol
		/// options instance. See <Security/SecProtocolOptions.h> for functions
		/// to further configure security options.
		public var securityProtocolOptions: sec_protocol_options_t {
			return nw_tls_copy_sec_protocol_options(self.nw)
		}

		/// Create TLS options to set in an NWParameters.ProtocolStack
		public init() {
			super.init(nw_tls_create_options())
		}

		override internal init(_ nw: nw_protocol_options_t) {
			super.init(nw)
		}
	}

	/// Access TLS metadata using NWConnection.metadata(protocolDefinition: NWProtocolTLS.definition)
	/// or in received ContentContext
	public class Metadata: NWProtocolMetadata {
		/// Access the sec_protocol_metadata_t for a given network protocol
		/// metadata instance. See <Security/SecProtocolMetadata.h> for functions
		/// to further access security metadata.
		public var securityProtocolMetadata: sec_protocol_metadata_t {
			return nw_tls_copy_sec_protocol_metadata(self.nw)
		}

		override internal init(_ nw: nw_protocol_metadata_t) {
			super.init(nw)
		}
	}
}
