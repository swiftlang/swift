
let NSHTTPCookieName: String
let NSHTTPCookieValue: String
let NSHTTPCookieOriginURL: String
let NSHTTPCookieVersion: String
let NSHTTPCookieDomain: String
let NSHTTPCookiePath: String
let NSHTTPCookieSecure: String
let NSHTTPCookieExpires: String
let NSHTTPCookieComment: String
let NSHTTPCookieCommentURL: String
let NSHTTPCookieDiscard: String
let NSHTTPCookieMaximumAge: String
let NSHTTPCookiePort: String
class NSHTTPCookie : NSObject {
  init?(properties properties: [String : AnyObject])
  @discardableResult
  class func requestHeaderFields(with cookies: [NSHTTPCookie]) -> [String : String]
  @discardableResult
  class func cookies(withResponseHeaderFields headerFields: [String : String], for URL: NSURL) -> [NSHTTPCookie]
  var properties: [String : AnyObject]? { get }
  var version: Int { get }
  var name: String { get }
  var value: String { get }
  @NSCopying var expiresDate: NSDate? { get }
  var isSessionOnly: Bool { get }
  var domain: String { get }
  var path: String { get }
  var isSecure: Bool { get }
  var isHTTPOnly: Bool { get }
  var comment: String? { get }
  @NSCopying var commentURL: NSURL? { get }
  var portList: [NSNumber]? { get }
}
