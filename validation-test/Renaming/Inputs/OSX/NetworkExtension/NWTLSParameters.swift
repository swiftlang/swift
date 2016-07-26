
class NWTLSParameters : NSObject {
  @available(OSX 10.11, *)
  @NSCopying var tlsSessionID: NSData?
  @available(OSX 10.11, *)
  var sslCipherSuites: Set<NSNumber>?
  @available(OSX 10.11, *)
  var minimumSSLProtocolVersion: Int
  @available(OSX 10.11, *)
  var maximumSSLProtocolVersion: Int
}
