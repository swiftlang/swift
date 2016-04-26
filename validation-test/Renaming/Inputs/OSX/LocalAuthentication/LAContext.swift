
@available(OSX 10.10, *)
enum LAPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX 10.11, *)
  case deviceOwnerAuthentication
}
@available(OSX 10.10, *)
class LAContext : NSObject {
  @discardableResult
  func canEvaluatePolicy(_ policy: LAPolicy, error error: NSErrorPointer) -> Bool
  func evaluatePolicy(_ policy: LAPolicy, localizedReason localizedReason: String, reply reply: (Bool, NSError?) -> Void)
  @available(OSX 10.11, *)
  func invalidate()
  @available(OSX 10.11, *)
  @discardableResult
  func setCredential(_ credential: NSData?, type type: LACredentialType) -> Bool
  @available(OSX 10.11, *)
  @discardableResult
  func isCredentialSet(_ type: LACredentialType) -> Bool
  @available(OSX 10.11, *)
  func evaluateAccessControl(_ accessControl: SecAccessControl, operation operation: LAAccessControlOperation, localizedReason localizedReason: String, reply reply: (Bool, NSError?) -> Void)
  var localizedFallbackTitle: String?
  @available(OSX 10.11, *)
  var evaluatedPolicyDomainState: NSData? { get }
}
@available(OSX 10.11, *)
enum LACredentialType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case applicationPassword
}
@available(OSX 10.11, *)
enum LAAccessControlOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case createItem
  case useItem
  case createKey
  case useKeySign
}
