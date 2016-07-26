
@available(OSX 10.10, *)
class NEVPNProtocol : NSObject, NSCopying, NSSecureCoding {
  @available(OSX 10.10, *)
  var serverAddress: String?
  @available(OSX 10.10, *)
  var username: String?
  @available(OSX 10.10, *)
  @NSCopying var passwordReference: NSData?
  @available(OSX 10.10, *)
  @NSCopying var identityReference: NSData?
  @available(OSX 10.11, *)
  @NSCopying var identityData: NSData?
  @available(OSX 10.11, *)
  var identityDataPassword: String?
  @available(OSX 10.10, *)
  var disconnectOnSleep: Bool
  @available(OSX 10.11, *)
  @NSCopying var proxySettings: NEProxySettings?
}
