
enum NSURLCredentialPersistence : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case forSession
  case permanent
  @available(OSX 10.8, *)
  case synchronizable
}
class NSURLCredential : NSObject, NSSecureCoding, NSCopying {
  var persistence: NSURLCredentialPersistence { get }
}
extension NSURLCredential {
  init(user user: String, password password: String, persistence persistence: NSURLCredentialPersistence)
  var user: String? { get }
  var password: String? { get }
  var hasPassword: Bool { get }
}
extension NSURLCredential {
  @available(OSX 10.6, *)
  init(identity identity: SecIdentity, certificates certArray: [AnyObject]?, persistence persistence: NSURLCredentialPersistence)
  var identity: SecIdentity? { get }
  @available(OSX 10.6, *)
  var certificates: [AnyObject] { get }
}
extension NSURLCredential {
  @available(OSX 10.6, *)
  init(trust trust: SecTrust)
}
