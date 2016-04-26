
var ODPacketSigningDisabled: Int { get }
var ODPacketSigningAllow: Int { get }
var ODPacketSigningRequired: Int { get }
var ODPacketEncryptionDisabled: Int { get }
var ODPacketEncryptionAllow: Int { get }
var ODPacketEncryptionRequired: Int { get }
var ODPacketEncryptionSSL: Int { get }
@available(OSX 10.9, *)
let ODTrustTypeJoined: String
@available(OSX 10.9, *)
let ODTrustTypeUsingCredentials: String
@available(OSX 10.9, *)
let ODTrustTypeAnonymous: String
@available(OSX 10.9, *)
class ODConfiguration : NSObject {
  @available(OSX 10.9, *)
  var nodeName: String!
  @available(OSX 10.9, *)
  var comment: String!
  @available(OSX 10.9, *)
  var defaultMappings: ODMappings!
  @available(OSX 10.9, *)
  var templateName: String!
  @available(OSX 10.9, *)
  var virtualSubnodes: [AnyObject]!
  @available(OSX 10.9, *)
  var hideRegistration: Bool
  @available(OSX 10.9, *)
  var preferredDestinationHostName: String!
  @available(OSX 10.9, *)
  var preferredDestinationHostPort: UInt16
  @available(OSX 10.9, *)
  var trustAccount: String! { get }
  @available(OSX 10.9, *)
  var trustMetaAccount: String! { get }
  @available(OSX 10.9, *)
  var trustKerberosPrincipal: String! { get }
  @available(OSX 10.9, *)
  var trustType: String! { get }
  @available(OSX 10.9, *)
  var trustUsesMutualAuthentication: Bool { get }
  @available(OSX 10.9, *)
  var trustUsesKerberosKeytab: Bool { get }
  @available(OSX 10.9, *)
  var trustUsesSystemKeychain: Bool { get }
  @available(OSX 10.9, *)
  var packetSigning: Int
  @available(OSX 10.9, *)
  var packetEncryption: Int
  @available(OSX 10.9, *)
  var manInTheMiddleProtection: Bool
  @available(OSX 10.9, *)
  var queryTimeoutInSeconds: Int
  @available(OSX 10.9, *)
  var connectionSetupTimeoutInSeconds: Int
  @available(OSX 10.9, *)
  var connectionIdleTimeoutInSeconds: Int
  @available(OSX 10.9, *)
  var defaultModuleEntries: [AnyObject]!
  @available(OSX 10.9, *)
  var authenticationModuleEntries: [AnyObject]!
  @available(OSX 10.9, *)
  var discoveryModuleEntries: [AnyObject]!
  @available(OSX 10.9, *)
  var generalModuleEntries: [AnyObject]!
  @available(OSX 10.9, *)
  @discardableResult
  class func suggestedTrustAccount(_ hostname: String!) -> String!
  @available(OSX 10.9, *)
  @discardableResult
  class func suggestedTrustPassword(_ length: Int) -> String!
  @available(OSX 10.9, *)
  func addTrustType(_ trustType: String!, trustAccount account: String!, trustPassword accountPassword: String!, username username: String!, password password: String!, joinExisting join: Bool) throws
  @available(OSX 10.9, *)
  func removeTrust(usingUsername username: String!, password password: String!, deleteTrustAccount deleteAccount: Bool) throws
}
