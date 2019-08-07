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

import Dispatch
import Foundation
import _SwiftNetworkOverlayShims

/// An NWConnection is an object that represents a bi-directional data pipe between
/// a local endpoint and a remote endpoint.
///
/// A connection handles establishment of any transport, security, or application-level protocols
/// required to transmit and receive user data. Multiple protocol instances may be attempted during
/// the establishment phase of the connection.
// NOTE: older overlays had Network.NWConnection as the ObjC name.
// The two must coexist, so it was renamed. The old name must not be
// used in the new runtime. _TtC7Network13_NWConnection is the
// mangled name for Network._NWConnection.
@_objcRuntimeName(_TtC7Network13_NWConnection)
@available(macOS 10.14, iOS 12.0, watchOS 5.0, tvOS 12.0, *)
public final class NWConnection : CustomDebugStringConvertible {

	public var debugDescription: String {
		return String("\(self.nw)")
	}

	/// States a connection may be in
	public enum State : Equatable {
		/// The initial state prior to start
		case setup
		/// Waiting connections have not yet been started, or do not have a viable network
		case waiting(NWError)
		/// Preparing connections are actively establishing the connection
		case preparing
		/// Ready connections can send and receive data
		case ready
		/// Failed connections are disconnected and can no longer send or receive data
		case failed(NWError)
		/// Cancelled connections have been invalidated by the client and will send no more events
		case cancelled

		internal init(_ nw: nw_connection_state_t, _ err: nw_error_t?) {
			switch nw {
			case Network.nw_connection_state_invalid:
				self = .setup
			case Network.nw_connection_state_waiting:
				if let error = NWError(err) {
					self = .waiting(error)
				} else {
					self = .waiting(NWError.posix(.ENETDOWN))
				}
			case Network.nw_connection_state_preparing:
				self = .preparing
			case Network.nw_connection_state_ready:
				self = .ready
			case Network.nw_connection_state_failed:
				if let error = NWError(err) {
					self = .failed(error)
				} else {
					self = .failed(NWError.posix(.EINVAL))
				}
			case Network.nw_connection_state_cancelled:
				self = .cancelled
			default:
				self = .cancelled
			}
		}
	}

	internal let nw : nw_connection_t

	private var _state = NWConnection.State.setup

	/// Access the current state of the connection
	public var state: NWConnection.State {
		return _state
	}

	private var _stateUpdateHandler: ((_ state: NWConnection.State) -> Void)?

	/// Set a block to be called when the connection's state changes, which may be called
	/// multiple times until the connection is cancelled.
	public var stateUpdateHandler: ((_ state: NWConnection.State) -> Void)? {
		set {
			self._stateUpdateHandler = newValue
			if let newValue = newValue {
				nw_connection_set_state_changed_handler(self.nw) { (state, error) in
					self._state = NWConnection.State(state, error)
					newValue(self._state)
				}
			} else {
				nw_connection_set_state_changed_handler(self.nw) { (state, error) in
					self._state = NWConnection.State(state, error)
				}
			}
		}
		get {
			return self._stateUpdateHandler
		}
	}

	/// Retrieve the maximum datagram size that can be sent
	/// on the connection. Any datagrams sent should be less
	/// than or equal to this size.
	@available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *)
	public var maximumDatagramSize: Int {
		get {
			return Int(nw_connection_get_maximum_datagram_size(self.nw))
		}
	}

	private var _currentPath: NWPath? = nil

	/// Current path for the connection, which can be used to extract interface and effective endpoint information
	public var currentPath: NWPath? {
		get {
			if let path = nw_connection_copy_current_path(self.nw) {
				return NWPath(path)
			}
			return nil
		}
	}

	private var _pathUpdateHandler: ((_ newPath: NWPath) -> Void)?

	/// Set a block to be called when the connection's path has changed, which may be called
	/// multiple times until the connection is cancelled.
	public var pathUpdateHandler: ((_ newPath: NWPath) -> Void)? {
		set {
			self._pathUpdateHandler = newValue
			if let newValue = newValue {
				nw_connection_set_path_changed_handler(self.nw) { (nwPath) in
					let newPath = NWPath(nwPath)
					self._currentPath = newPath
					newValue(newPath)
				}
			} else {
				nw_connection_set_path_changed_handler(self.nw, nil)
			}
			
		}
		get {
			return self._pathUpdateHandler
		}
	}

	private var _viabilityUpdateHandler: ((_ isViable: Bool) -> Void)?

	/// Set a block to be called when the connection's viability changes, which may be called
	/// multiple times until the connection is cancelled.
	///
	/// Connections that are not currently viable do not have a route, and packets will not be
	/// sent or received. There is a possibility that the connection will become viable
	/// again when network connectivity changes.
	public var viabilityUpdateHandler: ((_ isViable: Bool) -> Void)? {
		set {
			self._viabilityUpdateHandler = newValue
			if let newValue = newValue {
				nw_connection_set_viability_changed_handler(self.nw) { (isViable) in
					newValue(isViable)
				}
			} else {
				nw_connection_set_viability_changed_handler(self.nw, nil)
			}

		}
		get {
			return self._viabilityUpdateHandler
		}
	}

	private var _betterPathUpdateHandler: ((_ betterPathAvailable: Bool) -> Void)?

	/// A better path being available indicates that the system thinks there is a preferred path or
	/// interface to use, compared to the one this connection is actively using. As an example, the
	/// connection is established over an expensive cellular interface and an unmetered Wi-Fi interface
	/// is now available.
	///
	/// Set a block to be called when a better path becomes available or unavailable, which may be called
	/// multiple times until the connection is cancelled.
	///
	/// When a better path is available, if it is possible to migrate work from this connection to a new connection,
	/// create a new connection to the endpoint. Continue doing work on this connection until the new connection is
	/// ready. Once ready, transition work to the new connection and cancel this one.
	public var betterPathUpdateHandler: ((_ betterPathAvailable: Bool) -> Void)? {
		set {
			self._betterPathUpdateHandler = newValue
			if let newValue = newValue {
				nw_connection_set_better_path_available_handler(self.nw) { (betterPathAvailable) in
					newValue(betterPathAvailable)
				}
			} else {
				nw_connection_set_better_path_available_handler(self.nw, nil)
			}

		}
		get {
			return self._betterPathUpdateHandler
		}
	}

	/// The remote endpoint of the connection
	public let endpoint: NWEndpoint

	/// The set of parameters with which the connection was created
	public let parameters: NWParameters

	private init(to: NWEndpoint, using: NWParameters, connection: nw_connection_t) {
		self.endpoint = to
		self.parameters = using
		self.nw = connection
		nw_connection_set_state_changed_handler(self.nw) { (state, error) in
			self._state = NWConnection.State(state, error)
		}
	}

	convenience internal init?(using: NWParameters, inbound: nw_connection_t) {
		guard let peer = NWEndpoint(nw_connection_copy_endpoint(inbound)) else {
			return nil
		}
		self.init(to: peer, using: using, connection: inbound)
	}

	/// Create a new outbound connection to an endpoint, with parameters.
	/// The parameters determine the protocols to be used for the connection, and their options.
	///
	/// - Parameter to: The remote endpoint to which to connect.
	/// - Parameter using: The parameters define which protocols and path to use.
	public init(to: NWEndpoint, using: NWParameters) {
		self.endpoint = to
		self.parameters = using
		self.nw = nw_connection_create(to.nw!, using.nw)
		nw_connection_set_state_changed_handler(self.nw) { (state, error) in
			self._state = NWConnection.State(state, error)
		}
	}

	/// Create a new outbound connection to a hostname and port, with parameters.
	/// The parameters determine the protocols to be used for the connection, and their options.
	///
	/// - Parameter host: The remote hostname to which to connect.
	/// - Parameter port: The remote port to which to connect.
	/// - Parameter using: The parameters define which protocols and path to use.
	public convenience init(host: NWEndpoint.Host, port: NWEndpoint.Port, using: NWParameters) {
		self.init(to: .hostPort(host: host, port: port), using: using)
	}

	/// Start the connection and provide a dispatch queue for callback blocks.
	///
	/// Starts the connection, which will cause the connection to evaluate its path, do resolution and try to become
	/// ready (connected). NWConnection establishment is asynchronous. A stateUpdateHandler may be used to determine
	/// when the state changes. If the connection can not be established, the state will transition to waiting with
	/// an associated error describing the reason. If an unrecoverable error is encountered, the state will
	/// transition to failed with an associated NWError value. If the connection is established, the state will
	/// transition to ready.
 	///
	/// Start should only be called once on a connection, and multiple calls to start will
	/// be ignored.
	public func start(queue: DispatchQueue) {
		self._queue = queue
		nw_connection_set_queue(self.nw, queue)
		nw_connection_start(self.nw)
	}

	private var _queue: DispatchQueue?

	/// Get queue used for delivering block handlers, such as stateUpdateHandler, pathUpdateHandler,
	/// viabilityUpdateHandler, betterPathUpdateHandler, and completions for send and receive.
	/// If the connection has not yet been started, the queue will be nil. Once the connection has been
	/// started, the queue will be valid.
	public var queue: DispatchQueue? {
		get {
			return self._queue
		}
	}

	/// Cancel the connection, and cause all update handlers to be cancelled.
	///
	/// Cancel is asynchronous. The last callback will be to the stateUpdateHandler with the cancelled state. After
	/// the stateUpdateHandlers are called with the cancelled state, all blocks are released to break retain cycles.
    ///
    /// Calls to cancel after the first one are ignored.
	public func cancel() {
		nw_connection_cancel(self.nw)
	}

	/// A variant of cancel that performs non-graceful closure of the transport.
	public func forceCancel() {
		nw_connection_force_cancel(self.nw)
	}

	/// Cancel the currently connected endpoint, causing the connection to fall through to the next endpoint if
	/// available, or to go to the waiting state if no more endpoints are available.
	public func cancelCurrentEndpoint() {
		nw_connection_cancel_current_endpoint(self.nw)
	}

	/// NWConnections will normally re-attempt on network changes. This function causes a connection that is in
	/// the waiting state to re-attempt even without a network change.
	public func restart() {
		nw_connection_restart(self.nw)
	}

	/// Content Context controls options for how data is sent, and access for metadata to send or receive.
	/// All properties of Content Context are valid for sending. For received Content Context, only the protocolMetadata
	/// values will be set.
	// Set the ObjC name of this class to be nested in the customized ObjC name of NWConnection.
	@_objcRuntimeName(_TtCC7Network13_NWConnection14ContentContext)
	public class ContentContext {
        internal let nw: nw_content_context_t

        /// A string description of the content, used for logging and debugging.
        public let identifier: String

		/// An expiration in milliseconds after scheduling a send, after which the content may be dropped.
		/// Defaults to 0, which implies no expiration. Used only when sending.
		public let expirationMilliseconds: UInt64

		/// A numeric value between 0.0 and 1.0 to specify priority of this data/message. Defaults to 0.5.
		/// Used only when sending.
		public let relativePriority: Double

		/// Any content marked as an antecedent must be sent prior to this content being sent. Defaults to nil.
		/// Used only when sending.
		public let antecedent: NWConnection.ContentContext?

		/// A boolean indicating if this context is the final context in a given direction on this connection. Defaults to false.
		public let isFinal: Bool

		/// An array of protocol metadata to send (to inform the protocols of per-data options) or receive (to receive per-data options or statistics).
        public var protocolMetadata: [NWProtocolMetadata] {
            get {
                var metadataArray: [NWProtocolMetadata] = []
                nw_content_context_foreach_protocol_metadata(self.nw) { (definition, metadata) in
                    if nw_protocol_metadata_is_tcp(metadata) {
                       metadataArray.append(NWProtocolTCP.Metadata(metadata))
                    } else if nw_protocol_metadata_is_udp(metadata) {
                        metadataArray.append(NWProtocolUDP.Metadata(metadata))
                    } else if nw_protocol_metadata_is_ip(metadata) {
                        metadataArray.append(NWProtocolIP.Metadata(metadata))
					} else if nw_protocol_metadata_is_tls(metadata) {
						metadataArray.append(NWProtocolTLS.Metadata(metadata))
					}
                }
                return metadataArray
            }
        }

		/// Access the metadata for a specific protocol from a context. The metadata may be nil.
        public func protocolMetadata(definition: NWProtocolDefinition) -> NWProtocolMetadata? {
			if let metadata = nw_content_context_copy_protocol_metadata(self.nw, definition.nw) {
				if nw_protocol_metadata_is_tcp(metadata) {
					return NWProtocolTCP.Metadata(metadata)
				} else if nw_protocol_metadata_is_udp(metadata) {
					return NWProtocolUDP.Metadata(metadata)
				} else if nw_protocol_metadata_is_ip(metadata) {
					return NWProtocolIP.Metadata(metadata)
				} else if nw_protocol_metadata_is_tls(metadata) {
					return NWProtocolTLS.Metadata(metadata)
				}
			}
			return nil
        }

		/// Create a context for sending, that optionally can set expiration (default 0),
		/// priority (default 0.5), antecedent (default nil), and protocol metadata (default []]).
		public init(identifier: String, expiration: UInt64 = 0, priority: Double = 0.5, isFinal: Bool = false, antecedent: NWConnection.ContentContext? = nil, metadata: [NWProtocolMetadata]? = []) {
            self.nw = nw_content_context_create(identifier)

            self.identifier = identifier

			self.expirationMilliseconds = expiration
            nw_content_context_set_expiration_milliseconds(self.nw, expiration)

			self.relativePriority = priority
            nw_content_context_set_relative_priority(self.nw, priority)

			self.isFinal = isFinal
			nw_content_context_set_is_final(self.nw, isFinal)

			self.antecedent = antecedent
            if let otherContext = antecedent {
                nw_content_context_set_antecedent(self.nw, otherContext.nw)
            }

			if let metadataArray = metadata {
				for singleMetadata in metadataArray {
					nw_content_context_set_metadata_for_protocol(self.nw, singleMetadata.nw)
				}
			}
		}

		/// Use the default message context to send content with all default properties:
		/// default priority, no expiration, and not the final message. Marking this context
		/// as complete with a send indicates that the message content is now complete and any
		/// other messages that were blocked may be scheduled, but will not close the underlying
		/// connection. Use this context for any lightweight sends of datagrams or messages on
		/// top of a stream that do not require special properties.
		/// This context does not support overriding any properties.
		public static let defaultMessage : NWConnection.ContentContext = NWConnection.ContentContext(_swift_nw_content_context_default_message())!

		/// Use the final message context to indicate that no more sends are expected
		/// once this context is complete. Like .defaultMessage, all properties are default.
		/// Marking a send as complete when using this context will close the sending side of the
		/// underlying connection. This is the equivalent of sending a FIN on a TCP stream.
		/// This context does not support overriding any properties.
		public static let finalMessage : NWConnection.ContentContext = NWConnection.ContentContext(_swift_nw_content_context_final_message())!

		/// Use the default stream context to indicate that this sending context is
		/// the one that represents the entire connection. All context properties are default.
		/// This context behaves in the same way as .finalMessage, such that marking the
		/// context complete by sending isComplete will close the sending side of the
		/// underlying connection (a FIN for a TCP stream).
		/// Note that this context is a convenience for sending a single, final context.
		/// If the protocol used by the connection is a stream (such as TCP), the caller
		/// may still use .defaultMessage, .finalMessage, or a custom context with priorities
		/// and metadata to set properties of a particular chunk of stream data relative
		/// to other data on the stream.
		/// This context does not support overriding any properties.
		public static let defaultStream : NWConnection.ContentContext = NWConnection.ContentContext(_swift_nw_content_context_default_stream())!

		internal init?(_ nw: nw_content_context_t?) {
			guard let nw = nw else {
				return nil
			}

            self.nw = nw

			// Received content context doesn't have expiration, priority, or antecedents
			self.expirationMilliseconds = 0
			self.relativePriority = 0
			self.antecedent = nil
			self.isFinal = nw_content_context_get_is_final(nw)
            self.identifier = String(cString: nw_content_context_get_identifier(nw))
		}
	}

	/// Receive data from a connection. This may be called before the connection
	/// is ready, in which case the receive request will be queued until the
	/// connection is ready. The completion handler will be invoked exactly
	/// once for each call, so the client must call this function multiple
	/// times to receive multiple chunks of data. For protocols that
	/// support flow control, such as TCP, calling receive opens the receive
	/// window. If the client stops calling receive, the receive window will
	/// fill up and the remote peer will stop sending.
	/// - Parameter minimumIncompleteLength: The minimum length to receive from the connection,
	///   until the content is complete.
	/// - Parameter maximumLength: The maximum length to receive from the connection in a single completion.
	/// - Parameter completion: A receive completion is invoked exactly once for a call to receive(...).
	///   The completion indicates that the requested content has been received (in which case
	///   the content is delivered), or else an error has occurred. Parameters to the completion are:
	///
	///   - content: The received content, as constrained by the minimum and maximum length. This may
	///     be nil if the message or stream is complete (without any more data to deliver), or if
	///     an error was encountered.
	///
	///   - contentContext: Content context describing the received content. This includes protocol metadata
	///     that lets the caller introspect information about the received content (such as flags on a packet).
	///
	///   - isComplete: An indication that this context (a message or stream, for example) is now complete. For
	///     protocols such as TCP, this will be marked when the entire stream has be closed in the
	///     reading direction. For protocols such as UDP, this will be marked when the end of a
	///     datagram has been reached.
	///
	///   - error: An error will be sent if the receive was terminated before completing. There may still
	///     be content delivered along with the error, but this content may be shorter than the requested
	///     ranges. An error will be sent for any outstanding receives when the connection is cancelled.
	public func receive(minimumIncompleteLength: Int, maximumLength: Int,
						completion: @escaping (_ content: Data?,
											   _ contentContext: NWConnection.ContentContext?,
											   _ isComplete: Bool, _ error: NWError?) -> Void) {
		nw_connection_receive(self.nw, UInt32(minimumIncompleteLength), UInt32(maximumLength)) {
			(content, context, complete, nwError) in
			completion(NWCreateNSDataFromDispatchData(content) as Data?, ContentContext(context), complete, NWError(nwError));
		}
	}

	/// Receive complete message content from the connection, waiting for the content to be marked complete
	/// (or encounter an error) before delivering the callback. This is useful for datagram or message-based
	/// protocols like UDP. See receive(minimumIncompleteLength:, maximumLength:, completion:) for a description
	/// of the completion handler.
	public func receiveMessage(completion: @escaping (_ completeContent: Data?,
                                                      _ contentContext: NWConnection.ContentContext?,
                                                      _ isComplete: Bool, _ error: NWError?) -> Void) {
		nw_connection_receive_message(self.nw) { (content, context, complete, nwError) in
			completion(NWCreateNSDataFromDispatchData(content) as Data?, ContentContext(context), complete, NWError(nwError))
		}
	}

	/// A type representing a wrapped completion handler invoked when send content has been consumed by the protocol stack, or the lack of a completion handler because the content is idempotent.
	public enum SendCompletion {
		/// Completion handler to be invoked when send content has been successfully processed, or failed to send due to an error.
		/// Note that this does not guarantee that the data was sent out over the network, or acknowledge, but only that
		/// it has been consumed by the protocol stack.
		case contentProcessed((_ error: NWError?) -> Void)
		/// Idempotent content may be sent multiple times when opening up a 0-RTT connection, so there is no completion block
		case idempotent
	}

	/// Send data on a connection. This may be called before the connection is ready,
	/// in which case the send will be enqueued until the connection is ready to send.
	/// This is an asynchronous send and the completion block can be used to
	/// determine when the send is complete. There is nothing preventing a client
	/// from issuing an excessive number of outstanding sends. To minimize memory
	/// footprint and excessive latency as a consequence of buffer bloat, it is
	/// advisable to keep a low number of outstanding sends. The completion block
	/// can be used to pace subsequent sends.
	/// - Parameter content: The data to send on the connection. May be nil if this send marks its context as complete, such
	///   as by sending .finalMessage as the context and marking isComplete to send a write-close.
	/// - Parameter contentContext: The context associated with the content, which represents a logical message
	///   to be sent on the connection. All content sent within a single context will
	///   be sent as an in-order unit, up until the point that the context is marked
	///   complete (see isComplete). Once a context is marked complete, it may be re-used
	///   as a new logical message. Protocols like TCP that cannot send multiple
	///   independent messages at once (serial streams) will only start processing a new
	///   context once the prior context has been marked complete. Defaults to .defaultMessage.
	/// - Parameter isComplete: A flag indicating if the caller's sending context (logical message) is now complete.
	///   Until a context is marked complete, content sent for other contexts may not
	///   be sent immediately (if the protocol requires sending bytes serially, like TCP).
	///   For datagram protocols, like UDP, isComplete indicates that the content represents
	///   a complete datagram.
	///   When sending using streaming protocols like TCP, isComplete can be used to mark the end
    ///   of a single message on the stream, of which there may be many. However, it can also
	///   indicate that the connection should send a "write close" (a TCP FIN) if the sending
	///   context is the final context on the connection. Specifically, to send a "write close",
	///   pass .finalMessage or .defaultStream for the context (or create a custom context and
	///   set .isFinal), and pass true for isComplete.
	/// - Parameter completion: A completion handler (.contentProcessed) to notify the caller when content has been processed by
	///   the connection, or a marker that this data is idempotent (.idempotent) and may be sent multiple times as fast open data.
	public func send(content: Data?, contentContext: NWConnection.ContentContext = .defaultMessage, isComplete: Bool = true, completion: SendCompletion) {
		switch completion {
		case .idempotent:
			_swift_nw_connection_send_idempotent(self.nw, NWCreateDispatchDataFromNSData(content), contentContext.nw, isComplete)
		case .contentProcessed(let handler):
			_swift_nw_connection_send(self.nw, NWCreateDispatchDataFromNSData(content), contentContext.nw, isComplete) { (error) in
				handler(NWError(error))
			}
		}
	}

	/// Batching allows multiple send or receive calls provides a hint to the connection that the operations
	/// should be coalesced to improve efficiency. Calls other than send and receive will not be affected.
	public func batch(_ block: () -> Void) {
		nw_connection_batch(self.nw, block)
	}

	/// Access connection-wide protocol metadata on the connection. This allows access to state for protocols
	/// like TCP and TLS that have long-term state.
	public func metadata(definition: NWProtocolDefinition) -> NWProtocolMetadata? {
		if let metadata = nw_connection_copy_protocol_metadata(self.nw, definition.nw) {
			if nw_protocol_metadata_is_tcp(metadata) {
				return NWProtocolTCP.Metadata(metadata)
			} else if nw_protocol_metadata_is_udp(metadata) {
				return NWProtocolUDP.Metadata(metadata)
			} else if nw_protocol_metadata_is_ip(metadata) {
				return NWProtocolIP.Metadata(metadata)
			} else if nw_protocol_metadata_is_tls(metadata) {
				return NWProtocolTLS.Metadata(metadata)
			}
			return NWProtocolMetadata(metadata)
		} else {
			return nil
		}
	}
}
