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
import Security
import _SwiftNetworkOverlayShims

/// NWError is a type to deliver error codes relevant to NWConnection and NWListener objects.
/// Generic connectivity errors will be delivered in the posix domain, resolution errors will
/// be delivered in the dns domain, and security errors will be delivered in the tls domain.
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public enum NWError: Error, CustomDebugStringConvertible, Equatable {

	/// The error code will be a POSIX error as defined in <sys/errno.h>
	case posix(POSIXErrorCode)

	/// The error code will be a DNSServiceErrorType error as defined in <dns_sd.h>
	case dns(DNSServiceErrorType)

	/// The error code will be a TLS error as defined in <Security/SecureTransport.h>
	case tls(OSStatus)

	internal init(_ nw: nw_error_t) {
		switch nw_error_get_error_domain(nw) {
		case Network.nw_error_domain_posix:
			if let errorCode = POSIXErrorCode(rawValue: nw_error_get_error_code(nw)) {
				self = .posix(errorCode)
			} else {
				self = .posix(.EINVAL)
			}
		case Network.nw_error_domain_dns:
			self = .dns(DNSServiceErrorType(nw_error_get_error_code(nw)))
		case Network.nw_error_domain_tls:
			self = .tls(OSStatus(nw_error_get_error_code(nw)))
		default:
			self = .posix(.EINVAL)
		}
	}

	internal init?(_ nw: nw_error_t?) {
		guard let nw = nw else {
			return nil
		}
		self.init(nw)
	}

	public var debugDescription: String {
		switch self {
		case .posix(let posixError):
			let maxLen = 128
			let storage = UnsafeMutablePointer<Int8>.allocate(capacity: maxLen)
			var asString = "Unknown"
			if strerror_r(posixError.rawValue, storage, maxLen) == 0 {
				asString = String(cString: storage)
			}
			storage.deallocate()
			return String("\(posixError): \(asString)")
		case .dns(let dnsError):
			return String("\(dnsError): \(String(cString: nwlog_get_string_for_dns_service_error(Int32(dnsError))))")
		case .tls(let tlsError):
			return String("\(tlsError): \(String(describing: SecCopyErrorMessageString(Int32(tlsError), nil)))")
		}
	}
}
