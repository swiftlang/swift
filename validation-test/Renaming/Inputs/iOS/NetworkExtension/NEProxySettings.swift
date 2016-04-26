
@available(iOS 9.0, *)
class NEProxyServer : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(address address: String, port port: Int)
  @available(iOS 9.0, *)
  var address: String { get }
  @available(iOS 9.0, *)
  var port: Int { get }
  @available(iOS 9.0, *)
  var authenticationRequired: Bool
  @available(iOS 9.0, *)
  var username: String?
  @available(iOS 9.0, *)
  var password: String?
}
@available(iOS 9.0, *)
class NEProxySettings : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  var autoProxyConfigurationEnabled: Bool
  @available(iOS 9.0, *)
  @NSCopying var proxyAutoConfigurationURL: NSURL?
  @available(iOS 9.0, *)
  var proxyAutoConfigurationJavaScript: String?
  @available(iOS 9.0, *)
  var httpEnabled: Bool
  @available(iOS 9.0, *)
  @NSCopying var httpServer: NEProxyServer?
  @available(iOS 9.0, *)
  var httpsEnabled: Bool
  @available(iOS 9.0, *)
  @NSCopying var httpsServer: NEProxyServer?
  @available(iOS 9.0, *)
  var excludeSimpleHostnames: Bool
  @available(iOS 9.0, *)
  var exceptionList: [String]?
  @available(iOS 9.0, *)
  var matchDomains: [String]?
}
