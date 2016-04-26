
@available(OSX 10.10, *)
enum NEVPNIKEv2EncryptionAlgorithm : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case algorithmDES
  case algorithm3DES
  case algorithmAES128
  case algorithmAES256
  @available(OSX 10.11, *)
  case algorithmAES128GCM
  @available(OSX 10.11, *)
  case algorithmAES256GCM
}
@available(OSX 10.10, *)
enum NEVPNIKEv2IntegrityAlgorithm : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case SHA96
  case SHA160
  case SHA256
  case SHA384
  case SHA512
}
@available(OSX 10.10, *)
enum NEVPNIKEv2DeadPeerDetectionRate : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case low
  case medium
  case high
}
@available(OSX 10.10, *)
enum NEVPNIKEv2DiffieHellmanGroup : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case group0
  case group1
  case group2
  case group5
  case group14
  case group15
  case group16
  case group17
  case group18
  case group19
  case group20
  case group21
}
@available(OSX 10.11, *)
enum NEVPNIKEv2CertificateType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case RSA
  case ECDSA256
  case ECDSA384
  case ECDSA521
}
@available(OSX 10.10, *)
class NEVPNIKEv2SecurityAssociationParameters : NSObject, NSSecureCoding, NSCopying {
  @available(OSX 10.10, *)
  var encryptionAlgorithm: NEVPNIKEv2EncryptionAlgorithm
  @available(OSX 10.10, *)
  var integrityAlgorithm: NEVPNIKEv2IntegrityAlgorithm
  @available(OSX 10.10, *)
  var diffieHellmanGroup: NEVPNIKEv2DiffieHellmanGroup
  @available(OSX 10.10, *)
  var lifetimeMinutes: Int32
}
@available(OSX 10.10, *)
class NEVPNProtocolIKEv2 : NEVPNProtocolIPSec {
  @available(OSX 10.10, *)
  var deadPeerDetectionRate: NEVPNIKEv2DeadPeerDetectionRate
  @available(OSX 10.10, *)
  var serverCertificateIssuerCommonName: String?
  @available(OSX 10.10, *)
  var serverCertificateCommonName: String?
  @available(OSX 10.11, *)
  var certificateType: NEVPNIKEv2CertificateType
  @available(OSX 10.11, *)
  var useConfigurationAttributeInternalIPSubnet: Bool
  @available(OSX 10.10, *)
  var ikeSecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters { get }
  @available(OSX 10.10, *)
  var childSecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters { get }
  @available(OSX 10.11, *)
  var disableMOBIKE: Bool
  @available(OSX 10.11, *)
  var disableRedirect: Bool
  @available(OSX 10.11, *)
  var enablePFS: Bool
  @available(OSX 10.11, *)
  var enableRevocationCheck: Bool
  @available(OSX 10.11, *)
  var strictRevocationCheck: Bool
}
