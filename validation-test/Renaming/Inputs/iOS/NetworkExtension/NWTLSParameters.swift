
class NWTLSParameters : NSObject {
  @available(iOS 9.0, *)
  @NSCopying var tlsSessionID: NSData?
  @available(iOS 9.0, *)
  var sslCipherSuites: Set<NSNumber>?
  @available(iOS 9.0, *)
  var minimumSSLProtocolVersion: Int
  @available(iOS 9.0, *)
  var maximumSSLProtocolVersion: Int
}
