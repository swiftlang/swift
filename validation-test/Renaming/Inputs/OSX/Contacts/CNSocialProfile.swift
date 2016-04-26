
@available(OSX 10.11, *)
class CNSocialProfile : NSObject, NSCopying, NSSecureCoding {
  var urlString: String { get }
  var username: String { get }
  var userIdentifier: String { get }
  var service: String { get }
  init(urlString urlString: String?, username username: String?, userIdentifier userIdentifier: String?, service service: String?)
  @discardableResult
  class func localizedString(forKey key: String) -> String
  @discardableResult
  class func localizedString(forService service: String) -> String
}
@available(OSX 10.11, *)
let CNSocialProfileURLStringKey: String
@available(OSX 10.11, *)
let CNSocialProfileUsernameKey: String
@available(OSX 10.11, *)
let CNSocialProfileUserIdentifierKey: String
@available(OSX 10.11, *)
let CNSocialProfileServiceKey: String
@available(OSX 10.11, *)
let CNSocialProfileServiceFacebook: String
@available(OSX 10.11, *)
let CNSocialProfileServiceFlickr: String
@available(OSX 10.11, *)
let CNSocialProfileServiceLinkedIn: String
@available(OSX 10.11, *)
let CNSocialProfileServiceMySpace: String
@available(OSX 10.11, *)
let CNSocialProfileServiceSinaWeibo: String
@available(OSX 10.11, *)
let CNSocialProfileServiceTencentWeibo: String
@available(OSX 10.11, *)
let CNSocialProfileServiceTwitter: String
@available(OSX 10.11, *)
let CNSocialProfileServiceYelp: String
@available(OSX 10.11, *)
let CNSocialProfileServiceGameCenter: String
