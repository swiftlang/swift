
@available(iOS 8.0, *)
enum NEVPNIKEv2EncryptionAlgorithm : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case algorithmDES
  case algorithm3DES
  case algorithmAES128
  case algorithmAES256
  @available(iOS 8.3, *)
  case algorithmAES128GCM
  @available(iOS 8.3, *)
  case algorithmAES256GCM
}
@available(iOS 8.0, *)
enum NEVPNIKEv2IntegrityAlgorithm : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case SHA96
  case SHA160
  case SHA256
  case SHA384
  case SHA512
}
@available(iOS 8.0, *)
enum NEVPNIKEv2DeadPeerDetectionRate : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case low
  case medium
  case high
}
@available(iOS 8.0, *)
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
@available(iOS 8.3, *)
enum NEVPNIKEv2CertificateType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case RSA
  case ECDSA256
  case ECDSA384
  case ECDSA521
}
@available(iOS 8.0, *)
class NEVPNIKEv2SecurityAssociationParameters : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 8.0, *)
  var encryptionAlgorithm: NEVPNIKEv2EncryptionAlgorithm
  @available(iOS 8.0, *)
  var integrityAlgorithm: NEVPNIKEv2IntegrityAlgorithm
  @available(iOS 8.0, *)
  var diffieHellmanGroup: NEVPNIKEv2DiffieHellmanGroup
  @available(iOS 8.0, *)
  var lifetimeMinutes: Int32
}
@available(iOS 8.0, *)
class NEVPNProtocolIKEv2 : NEVPNProtocolIPSec {
  @available(iOS 8.0, *)
  var deadPeerDetectionRate: NEVPNIKEv2DeadPeerDetectionRate
  @available(iOS 8.0, *)
  var serverCertificateIssuerCommonName: String?
  @available(iOS 8.0, *)
  var serverCertificateCommonName: String?
  @available(iOS 8.3, *)
  var certificateType: NEVPNIKEv2CertificateType
  @available(iOS 9.0, *)
  var useConfigurationAttributeInternalIPSubnet: Bool
  @available(iOS 8.0, *)
  var ikeSecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters { get }
  @available(iOS 8.0, *)
  var childSecurityAssociationParameters: NEVPNIKEv2SecurityAssociationParameters { get }
  @available(iOS 9.0, *)
  var disableMOBIKE: Bool
  @available(iOS 9.0, *)
  var disableRedirect: Bool
  @available(iOS 9.0, *)
  var enablePFS: Bool
  @available(iOS 9.0, *)
  var enableRevocationCheck: Bool
  @available(iOS 9.0, *)
  var strictRevocationCheck: Bool
}
