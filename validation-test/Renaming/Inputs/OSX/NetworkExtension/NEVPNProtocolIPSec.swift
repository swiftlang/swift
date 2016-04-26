
@available(OSX 10.10, *)
enum NEVPNIKEAuthenticationMethod : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case certificate
  case sharedSecret
}
@available(OSX 10.10, *)
class NEVPNProtocolIPSec : NEVPNProtocol {
  @available(OSX 10.10, *)
  var authenticationMethod: NEVPNIKEAuthenticationMethod
  @available(OSX 10.10, *)
  var useExtendedAuthentication: Bool
  @available(OSX 10.10, *)
  @NSCopying var sharedSecretReference: NSData?
  @available(OSX 10.10, *)
  var localIdentifier: String?
  @available(OSX 10.10, *)
  var remoteIdentifier: String?
}
