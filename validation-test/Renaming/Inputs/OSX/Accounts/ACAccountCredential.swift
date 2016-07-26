
@available(OSX 10.8, *)
class ACAccountCredential : NSObject {
  init!(oAuthToken token: String!, tokenSecret secret: String!)
  init!(oAuth2Token token: String!, refreshToken refreshToken: String!, expiryDate expiryDate: NSDate!)
  var oauthToken: String!
}
