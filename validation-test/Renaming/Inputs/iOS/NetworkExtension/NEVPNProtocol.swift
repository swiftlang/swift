
@available(iOS 8.0, *)
class NEVPNProtocol : NSObject, NSCopying, NSSecureCoding {
  @available(iOS 8.0, *)
  var serverAddress: String?
  @available(iOS 8.0, *)
  var username: String?
  @available(iOS 8.0, *)
  @NSCopying var passwordReference: NSData?
  @available(iOS 9.0, *)
  @NSCopying var identityReference: NSData?
  @available(iOS 8.0, *)
  @NSCopying var identityData: NSData?
  @available(iOS 8.0, *)
  var identityDataPassword: String?
  @available(iOS 8.0, *)
  var disconnectOnSleep: Bool
  @available(iOS 9.0, *)
  @NSCopying var proxySettings: NEProxySettings?
}
