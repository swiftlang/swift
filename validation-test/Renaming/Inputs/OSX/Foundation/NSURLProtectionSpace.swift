
@available(OSX 10.5, *)
let NSURLProtectionSpaceHTTP: String
@available(OSX 10.5, *)
let NSURLProtectionSpaceHTTPS: String
@available(OSX 10.5, *)
let NSURLProtectionSpaceFTP: String
let NSURLProtectionSpaceHTTPProxy: String
let NSURLProtectionSpaceHTTPSProxy: String
let NSURLProtectionSpaceFTPProxy: String
let NSURLProtectionSpaceSOCKSProxy: String
let NSURLAuthenticationMethodDefault: String
let NSURLAuthenticationMethodHTTPBasic: String
let NSURLAuthenticationMethodHTTPDigest: String
let NSURLAuthenticationMethodHTMLForm: String
@available(OSX 10.5, *)
let NSURLAuthenticationMethodNTLM: String
@available(OSX 10.5, *)
let NSURLAuthenticationMethodNegotiate: String
@available(OSX 10.6, *)
let NSURLAuthenticationMethodClientCertificate: String
@available(OSX 10.6, *)
let NSURLAuthenticationMethodServerTrust: String
class NSURLProtectionSpace : NSObject, NSSecureCoding, NSCopying {
  init(host host: String, port port: Int, protocol protocol: String?, realm realm: String?, authenticationMethod authenticationMethod: String?)
  init(proxyHost host: String, port port: Int, type type: String?, realm realm: String?, authenticationMethod authenticationMethod: String?)
  var realm: String? { get }
  var receivesCredentialSecurely: Bool { get }
  var host: String { get }
  var port: Int { get }
  var proxyType: String? { get }
  var `protocol`: String? { get }
  var authenticationMethod: String { get }
}
extension NSURLProtectionSpace {
  @available(OSX 10.6, *)
  var distinguishedNames: [NSData]? { get }
}
extension NSURLProtectionSpace {
  @available(OSX 10.6, *)
  var serverTrust: SecTrust? { get }
}
