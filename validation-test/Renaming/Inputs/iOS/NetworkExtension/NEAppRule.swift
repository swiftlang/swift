
@available(iOS 9.0, *)
class NEAppRule : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  init(signingIdentifier signingIdentifier: String)
  @available(iOS 9.0, *)
  var matchSigningIdentifier: String { get }
  @available(iOS 9.3, *)
  var matchPath: String?
  @available(iOS 9.0, *)
  var matchDomains: [AnyObject]?
}
