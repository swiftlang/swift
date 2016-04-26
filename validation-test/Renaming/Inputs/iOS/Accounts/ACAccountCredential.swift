
@available(iOS 5.0, *)
class ACAccountCredential : NSObject {
  init!(oAuthToken token: String!, tokenSecret secret: String!)
  init!(oAuth2Token token: String!, refreshToken refreshToken: String!, expiryDate expiryDate: NSDate!)
  var oauthToken: String!
}
