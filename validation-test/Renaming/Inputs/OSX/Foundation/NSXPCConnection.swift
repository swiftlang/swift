
protocol NSXPCProxyCreating {
  @discardableResult
  func remoteObjectProxy() -> AnyObject
  @discardableResult
  func remoteObjectProxyWithErrorHandler(_ handler: (NSError) -> Void) -> AnyObject
}
@available(OSX 10.8, *)
struct NSXPCConnectionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var privileged: NSXPCConnectionOptions { get }
}
@available(OSX 10.8, *)
class NSXPCConnection : NSObject, NSXPCProxyCreating {
  init(serviceName serviceName: String)
  var serviceName: String? { get }
  init(machServiceName name: String, options options: NSXPCConnectionOptions = [])
  init(listenerEndpoint endpoint: NSXPCListenerEndpoint)
  var endpoint: NSXPCListenerEndpoint { get }
  var exportedInterface: NSXPCInterface?
  var exportedObject: AnyObject?
  var remoteObjectInterface: NSXPCInterface?
  var interruptionHandler: (() -> Void)?
  var invalidationHandler: (() -> Void)?
  func resume()
  func suspend()
  func invalidate()
  var auditSessionIdentifier: au_asid_t { get }
  var processIdentifier: pid_t { get }
  var effectiveUserIdentifier: uid_t { get }
  var effectiveGroupIdentifier: gid_t { get }
}
@available(OSX 10.8, *)
class NSXPCListener : NSObject {
  @discardableResult
  class func service() -> NSXPCListener
  @discardableResult
  class func anonymous() -> NSXPCListener
  init(machServiceName name: String)
  unowned(unsafe) var delegate: @sil_unmanaged NSXPCListenerDelegate?
  var endpoint: NSXPCListenerEndpoint { get }
  func resume()
  func suspend()
  func invalidate()
}
protocol NSXPCListenerDelegate : NSObjectProtocol {
  @available(OSX 10.8, *)
  @discardableResult
  optional func listener(_ listener: NSXPCListener, shouldAcceptNewConnection newConnection: NSXPCConnection) -> Bool
}
@available(OSX 10.8, *)
class NSXPCInterface : NSObject {
  /*not inherited*/ init(with protocol: Protocol)
  unowned(unsafe) var `protocol`: @sil_unmanaged Protocol
  func setClasses(_ classes: Set<NSObject>, for sel: Selector, argumentIndex arg: Int, ofReply ofReply: Bool)
  @discardableResult
  func classes(for sel: Selector, argumentIndex arg: Int, ofReply ofReply: Bool) -> Set<NSObject>
  func setInterface(_ ifc: NSXPCInterface, for sel: Selector, argumentIndex arg: Int, ofReply ofReply: Bool)
  @discardableResult
  func forSelector(_ sel: Selector, argumentIndex arg: Int, ofReply ofReply: Bool) -> NSXPCInterface?
}
@available(OSX 10.8, *)
class NSXPCListenerEndpoint : NSObject, NSSecureCoding {
}
