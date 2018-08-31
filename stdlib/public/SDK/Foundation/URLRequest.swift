//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

@available(*, deprecated, message: "Please use the struct type URLRequest")
public typealias MutableURLRequest = NSMutableURLRequest

public struct URLRequest : ReferenceConvertible, Equatable, Hashable {
    public typealias ReferenceType = NSURLRequest
    public typealias CachePolicy = NSURLRequest.CachePolicy
    public typealias NetworkServiceType = NSURLRequest.NetworkServiceType
    
    /*
     NSURLRequest has a fragile ivar layout that prevents the swift subclass approach here, so instead we keep an always mutable copy
    */
    internal var _handle: _MutableHandle<NSMutableURLRequest>
    
    internal mutating func _applyMutation<ReturnType>(_ whatToDo : (NSMutableURLRequest) -> ReturnType) -> ReturnType {
        if !isKnownUniquelyReferenced(&_handle) {
            let ref = _handle._uncopiedReference()
            _handle = _MutableHandle(reference: ref)
        }
        return whatToDo(_handle._uncopiedReference())
    }

    /// Creates and initializes a URLRequest with the given URL and cache policy.
    /// - parameter: url The URL for the request. 
    /// - parameter: cachePolicy The cache policy for the request. Defaults to `.useProtocolCachePolicy`
    /// - parameter: timeoutInterval The timeout interval for the request. See the commentary for the `timeoutInterval` for more information on timeout intervals. Defaults to 60.0
    public init(url: URL, cachePolicy: CachePolicy = .useProtocolCachePolicy, timeoutInterval: TimeInterval = 60.0) {
        _handle = _MutableHandle(adoptingReference: NSMutableURLRequest(url: url, cachePolicy: cachePolicy, timeoutInterval: timeoutInterval))
    }

    fileprivate init(_bridged request: NSURLRequest) {
        _handle = _MutableHandle(reference: request.mutableCopy() as! NSMutableURLRequest)
    }
    
    /// The URL of the receiver.
    public var url: URL? {
        get {
            return _handle.map { $0.url }
        }
        set {
            _applyMutation { $0.url = newValue }
        }
    }
    
    /// The cache policy of the receiver.
    public var cachePolicy: CachePolicy {
        get {
            return _handle.map { $0.cachePolicy }
        }
        set {
            _applyMutation { $0.cachePolicy = newValue }
        }
    }
    
    /// Returns the timeout interval of the receiver.
    /// - discussion: The timeout interval specifies the limit on the idle
    /// interval allotted to a request in the process of loading. The "idle
    /// interval" is defined as the period of time that has passed since the
    /// last instance of load activity occurred for a request that is in the
    /// process of loading. Hence, when an instance of load activity occurs
    /// (e.g. bytes are received from the network for a request), the idle
    /// interval for a request is reset to 0. If the idle interval ever
    /// becomes greater than or equal to the timeout interval, the request
    /// is considered to have timed out. This timeout interval is measured
    /// in seconds.
    public var timeoutInterval: TimeInterval {
        get {
            return _handle.map { $0.timeoutInterval }
        }
        set {
            _applyMutation { $0.timeoutInterval = newValue }
        }
    }
    
    /// The main document URL associated with this load.
    /// - discussion: This URL is used for the cookie "same domain as main
    /// document" policy.
    public var mainDocumentURL: URL? {
        get {
            return _handle.map { $0.mainDocumentURL }
        }
        set {
            _applyMutation { $0.mainDocumentURL = newValue }
        }
    }
    
    /// The URLRequest.NetworkServiceType associated with this request.
    /// - discussion: This will return URLRequest.NetworkServiceType.default for requests that have
    /// not explicitly set a networkServiceType
    @available(macOS 10.7, iOS 4.0, *)
    public var networkServiceType: NetworkServiceType {
        get {
            return _handle.map { $0.networkServiceType }
        }
        set {
            _applyMutation { $0.networkServiceType = newValue }
        }
    }
    
    /// `true` if the receiver is allowed to use the built in cellular radios to
    /// satisfy the request, `false` otherwise.
    @available(macOS 10.8, iOS 6.0, *)
    public var allowsCellularAccess: Bool {
        get {
            return _handle.map { $0.allowsCellularAccess }
        }
        set {
            _applyMutation { $0.allowsCellularAccess = newValue }
        }
    }
    
    /// The HTTP request method of the receiver.
    public var httpMethod: String? {
        get {
            return _handle.map { $0.httpMethod }
        }
        set {
            _applyMutation { 
                if let value = newValue {
                    $0.httpMethod = value
                } else {
                    $0.httpMethod = "GET"
                }
            }
        }
    }
    
    /// A dictionary containing all the HTTP header fields of the
    /// receiver.
    public var allHTTPHeaderFields: [String : String]? {
        get {
            return _handle.map { $0.allHTTPHeaderFields }
        }
        set {
            _applyMutation { $0.allHTTPHeaderFields = newValue }
        }
    }

    /// The value which corresponds to the given header
    /// field. Note that, in keeping with the HTTP RFC, HTTP header field
    /// names are case-insensitive.
    /// - parameter: field the header field name to use for the lookup (case-insensitive).
    public func value(forHTTPHeaderField field: String) -> String? {
        return _handle.map { $0.value(forHTTPHeaderField: field) }
    }
    
    /// If a value was previously set for the given header
    /// field, that value is replaced with the given value. Note that, in
    /// keeping with the HTTP RFC, HTTP header field names are
    /// case-insensitive.
    public mutating func setValue(_ value: String?, forHTTPHeaderField field: String) {
        _applyMutation {
            $0.setValue(value, forHTTPHeaderField: field)
        }
    }
    
    /// This method provides a way to add values to header
    /// fields incrementally. If a value was previously set for the given
    /// header field, the given value is appended to the previously-existing
    /// value. The appropriate field delimiter, a comma in the case of HTTP,
    /// is added by the implementation, and should not be added to the given
    /// value by the caller. Note that, in keeping with the HTTP RFC, HTTP
    /// header field names are case-insensitive.
    public mutating func addValue(_ value: String, forHTTPHeaderField field: String) {
        _applyMutation {
            $0.addValue(value, forHTTPHeaderField: field)
        }
    }
    
    /// This data is sent as the message body of the request, as
    /// in done in an HTTP POST request.
    public var httpBody: Data? {
        get {
            return _handle.map { $0.httpBody }
        }
        set {
            _applyMutation { $0.httpBody = newValue }
        }
    }
    
    /// The stream is returned for examination only; it is
    /// not safe for the caller to manipulate the stream in any way.  Also
    /// note that the HTTPBodyStream and HTTPBody are mutually exclusive - only
    /// one can be set on a given request.  Also note that the body stream is
    /// preserved across copies, but is LOST when the request is coded via the 
    /// NSCoding protocol
    public var httpBodyStream: InputStream? {
        get {
            return _handle.map { $0.httpBodyStream }
        }
        set {
            _applyMutation { $0.httpBodyStream = newValue }
        }
    }
    
    /// `true` if cookies will be sent with and set for this request; otherwise `false`.
    public var httpShouldHandleCookies: Bool {
        get {
            return _handle.map { $0.httpShouldHandleCookies }
        }
        set {
            _applyMutation { $0.httpShouldHandleCookies = newValue }
        }
    }
    
    /// `true` if the receiver should transmit before the previous response
    /// is received.  `false` if the receiver should wait for the previous response
    /// before transmitting.
    @available(macOS 10.7, iOS 4.0, *)
    public var httpShouldUsePipelining: Bool {
        get {
            return _handle.map { $0.httpShouldUsePipelining }
        }
        set {
            _applyMutation { $0.httpShouldUsePipelining = newValue }
        }
    }
    
    public var hashValue: Int {
        return _handle.map { $0.hashValue }
    }
    
    public static func ==(lhs: URLRequest, rhs: URLRequest) -> Bool {
        return lhs._handle._uncopiedReference().isEqual(rhs._handle._uncopiedReference())
    }
}

extension URLRequest : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {

    public var description: String {
      return url?.description ?? "url: nil"
    }
    
    public var debugDescription: String {
        return self.description
    }
    
    public var customMirror: Mirror {
        let c: [(label: String?, value: Any)] = [
          ("url", url as Any),
          ("cachePolicy", cachePolicy.rawValue),
          ("timeoutInterval", timeoutInterval),
          ("mainDocumentURL", mainDocumentURL as Any),
          ("networkServiceType", networkServiceType),
          ("allowsCellularAccess", allowsCellularAccess),
          ("httpMethod", httpMethod as Any),
          ("allHTTPHeaderFields", allHTTPHeaderFields as Any),
          ("httpBody", httpBody as Any),
          ("httpBodyStream", httpBodyStream as Any),
          ("httpShouldHandleCookies", httpShouldHandleCookies),
          ("httpShouldUsePipelining", httpShouldUsePipelining),
        ]
        return Mirror(self, children: c, displayStyle: Mirror.DisplayStyle.struct)
    }
}

extension URLRequest : _ObjectiveCBridgeable {
    public static func _getObjectiveCType() -> Any.Type {
        return NSURLRequest.self
    }
    
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSURLRequest {
        return _handle._copiedReference()
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSURLRequest, result: inout URLRequest?) {
        result = URLRequest(_bridged: input)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSURLRequest, result: inout URLRequest?) -> Bool {
        result = URLRequest(_bridged: input)
        return true
    }
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSURLRequest?) -> URLRequest {
        var result: URLRequest?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSURLRequest : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as URLRequest)
    }
}

