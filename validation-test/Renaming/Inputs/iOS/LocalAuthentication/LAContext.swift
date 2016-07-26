
@available(iOS 8.0, *)
enum LAPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(iOS 8.0, *)
  case deviceOwnerAuthenticationWithBiometrics
  @available(iOS 9.0, *)
  case deviceOwnerAuthentication
}
@available(iOS 9.0, *)
let LATouchIDAuthenticationMaximumAllowableReuseDuration: NSTimeInterval
@available(iOS 8.0, *)
class LAContext : NSObject {
  @discardableResult
  func canEvaluatePolicy(_ policy: LAPolicy, error error: NSErrorPointer) -> Bool
  func evaluatePolicy(_ policy: LAPolicy, localizedReason localizedReason: String, reply reply: (Bool, NSError?) -> Void)
  @available(iOS 9.0, *)
  func invalidate()
  @available(iOS 9.0, *)
  @discardableResult
  func setCredential(_ credential: NSData?, type type: LACredentialType) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  func isCredentialSet(_ type: LACredentialType) -> Bool
  @available(iOS 9.0, *)
  func evaluateAccessControl(_ accessControl: SecAccessControl, operation operation: LAAccessControlOperation, localizedReason localizedReason: String, reply reply: (Bool, NSError?) -> Void)
  var localizedFallbackTitle: String?
  @available(iOS, introduced: 8.3, deprecated: 9.0)
  var maxBiometryFailures: NSNumber?
  @available(iOS 9.0, *)
  var evaluatedPolicyDomainState: NSData? { get }
  @available(iOS 9.0, *)
  var touchIDAuthenticationAllowableReuseDuration: NSTimeInterval
}
@available(iOS 9.0, *)
enum LACredentialType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case applicationPassword
}
@available(iOS 9.0, *)
enum LAAccessControlOperation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case createItem
  case useItem
  case createKey
  case useKeySign
}
