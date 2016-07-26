
@available(OSX 10.8, *)
let ACAccountTypeIdentifierTwitter: String
@available(OSX 10.8, *)
let ACAccountTypeIdentifierFacebook: String
@available(OSX 10.8, *)
let ACAccountTypeIdentifierSinaWeibo: String
@available(OSX 10.9, *)
let ACAccountTypeIdentifierTencentWeibo: String
@available(OSX 10.9, *)
let ACAccountTypeIdentifierLinkedIn: String
@available(OSX 10.8, *)
let ACFacebookAppIdKey: String
@available(OSX 10.8, *)
let ACFacebookPermissionsKey: String
@available(OSX 10.8, *)
let ACFacebookAudienceKey: String
@available(OSX 10.8, *)
let ACFacebookAudienceEveryone: String
@available(OSX 10.8, *)
let ACFacebookAudienceFriends: String
@available(OSX 10.8, *)
let ACFacebookAudienceOnlyMe: String
@available(OSX 10.9, *)
let ACLinkedInAppIdKey: String
@available(OSX 10.9, *)
let ACLinkedInPermissionsKey: String
@available(OSX 10.9, *)
let ACTencentWeiboAppIdKey: String
@available(OSX 10.8, *)
class ACAccountType : NSObject {
  var accountTypeDescription: String! { get }
  var identifier: String! { get }
  var accessGranted: Bool { get }
}
