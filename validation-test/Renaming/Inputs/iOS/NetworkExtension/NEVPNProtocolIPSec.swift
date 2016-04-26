
@available(iOS 8.0, *)
enum NEVPNIKEAuthenticationMethod : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case certificate
  case sharedSecret
}
@available(iOS 8.0, *)
class NEVPNProtocolIPSec : NEVPNProtocol {
  @available(iOS 8.0, *)
  var authenticationMethod: NEVPNIKEAuthenticationMethod
  @available(iOS 8.0, *)
  var useExtendedAuthentication: Bool
  @available(iOS 8.0, *)
  @NSCopying var sharedSecretReference: NSData?
  @available(iOS 8.0, *)
  var localIdentifier: String?
  @available(iOS 8.0, *)
  var remoteIdentifier: String?
}
