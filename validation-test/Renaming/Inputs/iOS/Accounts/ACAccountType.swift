
@available(iOS 5.0, *)
let ACAccountTypeIdentifierTwitter: String
@available(iOS 6.0, *)
let ACAccountTypeIdentifierFacebook: String
@available(iOS 6.0, *)
let ACAccountTypeIdentifierSinaWeibo: String
@available(iOS 7.0, *)
let ACAccountTypeIdentifierTencentWeibo: String
@available(iOS 6.0, *)
let ACFacebookAppIdKey: String
@available(iOS 6.0, *)
let ACFacebookPermissionsKey: String
@available(iOS 6.0, *)
let ACFacebookAudienceKey: String
@available(iOS 6.0, *)
let ACFacebookAudienceEveryone: String
@available(iOS 6.0, *)
let ACFacebookAudienceFriends: String
@available(iOS 6.0, *)
let ACFacebookAudienceOnlyMe: String
@available(iOS 7.0, *)
let ACTencentWeiboAppIdKey: String
@available(iOS 5.0, *)
class ACAccountType : NSObject {
  var accountTypeDescription: String! { get }
  var identifier: String! { get }
  var accessGranted: Bool { get }
}
