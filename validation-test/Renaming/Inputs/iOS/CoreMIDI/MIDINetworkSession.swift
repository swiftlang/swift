
@available(iOS 4.2, *)
let MIDINetworkBonjourServiceType: String
@available(iOS 4.2, *)
let MIDINetworkNotificationContactsDidChange: String
@available(iOS 4.2, *)
let MIDINetworkNotificationSessionDidChange: String
enum MIDINetworkConnectionPolicy : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noOne
  case hostsInContactList
  case anyone
}
@available(iOS 4.2, *)
class MIDINetworkHost : NSObject {
  convenience init(name name: String, address address: String, port port: Int)
  convenience init(name name: String, netService netService: NSNetService)
  convenience init(name name: String, netServiceName netServiceName: String, netServiceDomain netServiceDomain: String)
  @discardableResult
  func hasSameAddress(as other: MIDINetworkHost) -> Bool
  var name: String { get }
  var address: String { get }
  var port: Int { get }
  var netServiceName: String? { get }
  var netServiceDomain: String? { get }
}
@available(iOS 4.2, *)
class MIDINetworkConnection : NSObject {
  convenience init(host host: MIDINetworkHost)
  var host: MIDINetworkHost { get }
}
@available(iOS 4.2, *)
class MIDINetworkSession : NSObject {
  @discardableResult
  class func defaultSession() -> MIDINetworkSession
  var isEnabled: Bool
  var networkPort: Int { get }
  var networkName: String { get }
  var localName: String { get }
  var connectionPolicy: MIDINetworkConnectionPolicy
  @discardableResult
  func contacts() -> Set<MIDINetworkHost>
  @discardableResult
  func addContact(_ contact: MIDINetworkHost) -> Bool
  @discardableResult
  func removeContact(_ contact: MIDINetworkHost) -> Bool
  @discardableResult
  func connections() -> Set<MIDINetworkConnection>
  @discardableResult
  func addConnection(_ connection: MIDINetworkConnection) -> Bool
  @discardableResult
  func removeConnection(_ connection: MIDINetworkConnection) -> Bool
  @discardableResult
  func sourceEndpoint() -> MIDIEndpointRef
  @discardableResult
  func destinationEndpoint() -> MIDIEndpointRef
}
