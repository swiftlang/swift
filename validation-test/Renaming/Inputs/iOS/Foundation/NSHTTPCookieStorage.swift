
enum NSHTTPCookieAcceptPolicy : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case always
  case never
  case onlyFromMainDocumentDomain
}
class NSHTTPCookieStorage : NSObject {
  @discardableResult
  class func shared() -> NSHTTPCookieStorage
  @available(iOS 9.0, *)
  @discardableResult
  class func sharedCookieStorage(forGroupContainerIdentifier identifier: String) -> NSHTTPCookieStorage
  var cookies: [NSHTTPCookie]? { get }
  func setCookie(_ cookie: NSHTTPCookie)
  func deleteCookie(_ cookie: NSHTTPCookie)
  @available(iOS 8.0, *)
  func removeCookies(since date: NSDate)
  @discardableResult
  func cookies(for URL: NSURL) -> [NSHTTPCookie]?
  func setCookies(_ cookies: [NSHTTPCookie], for URL: NSURL?, mainDocumentURL mainDocumentURL: NSURL?)
  var cookieAcceptPolicy: NSHTTPCookieAcceptPolicy
  @available(iOS 5.0, *)
  @discardableResult
  func sortedCookies(using sortOrder: [NSSortDescriptor]) -> [NSHTTPCookie]
}
extension NSHTTPCookieStorage {
  @available(iOS 8.0, *)
  func storeCookies(_ cookies: [NSHTTPCookie], for task: NSURLSessionTask)
  @available(iOS 8.0, *)
  func getCookiesFor(_ task: NSURLSessionTask, completionHandler completionHandler: ([NSHTTPCookie]?) -> Void)
}
let NSHTTPCookieManagerAcceptPolicyChangedNotification: String
let NSHTTPCookieManagerCookiesChangedNotification: String
