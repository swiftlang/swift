
@available(OSX 10.5, *)
class CBIdentity : NSObject, NSCoding, NSCopying {
  /*not inherited*/ init?(name name: String, authority authority: CBIdentityAuthority)
  @available(OSX 10.11, *)
  /*not inherited*/ init?(uniqueIdentifier uuid: NSUUID, authority authority: CBIdentityAuthority)
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use +identityWithUniqueIdentifier:authority: instead.")
  /*not inherited*/ init?(uuidString uuid: String, authority authority: CBIdentityAuthority)
  /*not inherited*/ init?(persistentReference data: NSData)
  var authority: CBIdentityAuthority { get }
  @available(OSX 10.11, *)
  var uniqueIdentifier: NSUUID { get }
  @available(OSX, introduced: 10.5, deprecated: 10.11, message: "Use the uniqueIdentifier property instead.")
  var uuidString: String { get }
  var fullName: String { get }
  var posixName: String { get }
  var aliases: [String] { get }
  var emailAddress: String? { get }
  var image: NSImage? { get }
  var persistentReference: NSData? { get }
  var isHidden: Bool { get }
  @discardableResult
  func isMember(ofGroup group: CBGroupIdentity) -> Bool
}
@available(OSX 10.5, *)
class CBUserIdentity : CBIdentity, NSCoding, NSCopying {
  /*not inherited*/ init?(posixUID uid: uid_t, authority authority: CBIdentityAuthority)
  var posixUID: uid_t { get }
  var certificate: SecCertificate? { get }
  var isEnabled: Bool { get }
  @discardableResult
  func authenticate(withPassword password: String) -> Bool
}
@available(OSX 10.5, *)
class CBGroupIdentity : CBIdentity {
  /*not inherited*/ init?(posixGID gid: gid_t, authority authority: CBIdentityAuthority)
  var posixGID: gid_t { get }
  @available(OSX 10.11, *)
  var memberIdentities: [CBIdentity] { get }
}
