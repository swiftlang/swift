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
import Dispatch
import Foundation

/// An NWListener is an object that is able to receive incoming NWConnection
/// objects by binding to a local endpoint. A listener will accept connections based
/// on the protocols defined in its stack. For transport listeners (such as TCP and UDP),
/// accepted connections will represent new local and remote address and port tuples.
/// For listeners that include higher-level protocols that support multiplexing,
/// accepted connections will represent multiplexed streams on a new or existing transport
/// binding.

@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public final class NWListener: CustomDebugStringConvertible {
	public var debugDescription: String {
		return String("\(self.nw)")
	}

 	/// Defines a service to advertise
	public struct Service : Equatable, CustomDebugStringConvertible {
		public var debugDescription: String {
			var description = ""
			if let name = self.name {
				description = String("\(name).\(type)")
			} else {
				description = String("*.\(type)")
			}
			if let domain = self.domain {
				description = String("\(description).\(domain)")
			} else {
				description = String("\(description).*")
			}
			if txtRecord != nil && txtRecord!.count > 0 {
				description = String("\(description) <\(txtRecord!.count) bytes of txt>")
			}
			return description
		}

		/// Bonjour service name - if nil the system name will be used
		public let name: String?

		/// Bonjour service type
		public let type: String

		/// Bonjour service domain - if nil the system will register in all appropriate domains
		public let domain: String?

		/// Bonjour txtRecord - metadata for the service. Update the txtRecord by setting the
		/// service on the listener with the same name/type/domain and a new txtRecord.
		public let txtRecord: Data?

		/// Create a Service to advertise for the listener. Name, domain, and txtRecord all default to nil.
		public init(name: String? = nil, type: String, domain: String? = nil, txtRecord: Data? = nil) {
			self.name = name
			self.type = type
			self.domain = domain
			self.txtRecord = txtRecord
		}

		internal var nw: nw_advertise_descriptor_t {
			get {
				let descriptor = nw_advertise_descriptor_create_bonjour_service(name, type, domain)
				if let txtRecord = txtRecord {
					txtRecord.withUnsafeBytes({ (ptr: UnsafePointer<UInt8>) -> Void in
						nw_advertise_descriptor_set_txt_record(descriptor!, ptr, txtRecord.count)
					})
				}
				return descriptor!
			}
		}
	}

	public enum State: Equatable {
		/// Prior to start, the listener will be in the setup state
		case setup

		/// Waiting listeners do not have a viable network
		case waiting(NWError)

		/// Ready listeners are able to receive incoming connections
 		/// Bonjour service may not yet be registered
		case ready

		/// Failed listeners are no longer able to receive incoming connections
		case failed(NWError)

		/// Cancelled listeners have been invalidated by the client and will send no more events
		case cancelled

		internal init(_ nw: nw_listener_state_t, _ err: nw_error_t?) {
			switch nw {
			case Network.nw_listener_state_invalid:
				self = .setup
			case Network.nw_listener_state_waiting:
				if let error = NWError(err) {
					self = .waiting(error)
				} else {
					self = .waiting(NWError.posix(.ENETDOWN))
				}
			case Network.nw_listener_state_ready:
				self = .ready
			case Network.nw_listener_state_failed:
				if let error = NWError(err) {
					self = .failed(error)
				} else {
					self = .failed(NWError.posix(.EINVAL))
				}
			case Network.nw_listener_state_cancelled:
				self = .cancelled
			default:
				self = .cancelled
			}
		}
	}

	private var _newConnectionHandler: ((_ connection: NWConnection) -> Void)?

	/// Block to be called for new inbound connections
	public var newConnectionHandler: ((_ connection: NWConnection) -> Void)? {
		set {
			self._newConnectionHandler = newValue
			if let newValue = newValue {
				nw_listener_set_new_connection_handler(self.nw) { (nwConnection) in
					guard let newConnection = NWConnection(using: self.parameters, inbound: nwConnection) else {
						return;
					}
					newValue(newConnection)
				}
			} else {
				nw_listener_set_state_changed_handler(self.nw, nil)
			}
		}
		get {
			return self._newConnectionHandler
		}
	}

	private var _state : State = .setup
	private let nw: nw_listener_t

	private var _stateUpdateHandler: ((_ state: NWListener.State) -> Void)?

	/// Set a block to be called when the listener's state changes, which may be called
	/// multiple times until the listener is cancelled.
	public var stateUpdateHandler: ((_ state: NWListener.State) -> Void)? {
		set {
			self._stateUpdateHandler = newValue
			if let newValue = newValue {
				nw_listener_set_state_changed_handler(self.nw) { (state, error) in
					self._state = NWListener.State(state, error)
					newValue(self._state)
				}
			} else {
				nw_listener_set_state_changed_handler(self.nw, nil)
			}
		}
		get {
			return self._stateUpdateHandler
		}
	}

	/// NWParameters used to create the listener
	public let parameters : NWParameters

	private var _service : NWListener.Service?

	/// Optional Bonjour service to advertise with the listener
	/// May be modified on the fly to update the TXT record or
	/// change the advertised service.
	public var service: NWListener.Service? {
		get {
			return _service
		}
		set {
			_service = newValue
			if let service = _service {
				nw_listener_set_advertise_descriptor(nw, service.nw)
			} else {
				nw_listener_set_advertise_descriptor(nw, nil)
			}
		}
	}

	/// The current port the listener is bound to, if any. The port is only valid when the listener is in the ready
	/// state.
	public var port: NWEndpoint.Port? {
		return NWEndpoint.Port(nw_listener_get_port(self.nw))
	}

	public enum ServiceRegistrationChange {
		/// An event when a Bonjour service has been registered, with the endpoint being advertised
		case add(NWEndpoint)

		/// An event when a Bonjour service has been unregistered, with the endpoint being removed
		case remove(NWEndpoint)
	}

	private var _serviceRegistrationUpdateHandler: ((_ change: NWListener.ServiceRegistrationChange) -> Void)?

	/// Set a block to be called when the listener has added or removed a
	/// registered service. This may be called multiple times until the listener
	/// is cancelled.
	public var serviceRegistrationUpdateHandler: ((_ change: NWListener.ServiceRegistrationChange) -> Void)? {
		set {
			self._serviceRegistrationUpdateHandler = newValue
			if let newValue = newValue {
				nw_listener_set_advertised_endpoint_changed_handler(self.nw) { (endpoint, added) in
					if let changedEndpoint = NWEndpoint(endpoint) {
					if added {
						newValue(NWListener.ServiceRegistrationChange.add(changedEndpoint))
					} else {
						newValue(NWListener.ServiceRegistrationChange.remove(changedEndpoint))
					}
					}
				}
			} else {
				nw_listener_set_advertised_endpoint_changed_handler(self.nw, nil)
			}
		}
		get {
			return self._serviceRegistrationUpdateHandler
		}
	}

	internal init(_ listener: nw_listener_t, _ parameters: NWParameters) {
		self.parameters = parameters;
		self.nw = listener
	}

	/// Creates a networking listener. The listener will be assigned a random
	/// port to listen on unless otherwise specified.
	///
	/// - Parameter using: The parameters to use for the listener, which include the protocols to use for the
	/// listener. The parameters requiredLocalEndpoint may be used to specify the local address and port to listen on.
	/// - Parameter on: The port to listen on. Defaults to .any which will cause a random unused port to be assigned.
	/// Specifying a port that is already in use will cause the listener to fail after starting.
	/// - Returns: Returns a listener object that may be set up and started or throws an error if the parameters are
	/// not compatible with the provided port.
	public init(using: NWParameters, on: NWEndpoint.Port = .any) throws {
		if on != .any {
			guard let listener = nw_listener_create_with_port("\(on)", using.nw) else {
				throw NWError.posix(.EINVAL)
			}
			self.parameters = using
			self.nw = listener
		} else {
			guard let listener = nw_listener_create(using.nw) else {
				throw NWError.posix(.EINVAL)
			}
			self.parameters = using
			self.nw = listener
		}
	}

	///	Creates a networking listener based on an existing
	///	multiplexing connection. If there are multiple protocols
	///	in the connection that support listening for incoming flows,
	///	the listener will be hooked up the highest in the stack
	///	(the closest to the reading and writing of the client data).
	///
	/// - Parameter withConnection: An existing connection that has a multiplexing protocol
	///		that supports receiving new connections.
	/// - Parameter parameters: The parameters to use for the listener. The protocol stack
	///		defined in the parameters must be able to join a protocol
	///		in the connection that supports listening protocols.
	convenience internal init?(connection: NWConnection, parameters: NWParameters) {
		guard let listener = nw_listener_create_with_connection(connection.nw, parameters.nw) else {
			return nil
		}
		self.init(listener, parameters)
	}

	/// Start the listener and provide a queue on which callback handlers will be executed.
	/// When the listener state moves to ready, the listener is registered with the system
	/// and can receive incoming connections.
	/// Start should only be called once on a listener, and multiple calls to start will
	/// be ignored.
	public func start(queue: DispatchQueue) {
		self._queue = queue
		nw_listener_set_queue(self.nw, queue)
		nw_listener_start(self.nw)
	}

	private var _queue : DispatchQueue? = nil

	/// Get queue used for delivering block handlers, such as stateUpdateHandler,
	/// serviceRegistrationUpdateHandler, and newConnectionHandler.
	/// If the listener has not yet been started, the queue will be nil. Once the listener has been
	/// started, the queue will be valid.
	public var queue: DispatchQueue? {
		get {
			return _queue
		}
	}

	/// Cancel the listener.
	///
	/// The cancel is asynchronous and the state will eventually transition to cancelled. Subsequent calls to
	/// cancel are ignored.
	public func cancel() {
		nw_listener_cancel(self.nw)
	}
}

