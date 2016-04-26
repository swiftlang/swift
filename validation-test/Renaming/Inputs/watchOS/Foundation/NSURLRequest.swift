
enum NSURLRequestCachePolicy : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case useProtocolCachePolicy
  case reloadIgnoringLocalCacheData
  case reloadIgnoringLocalAndRemoteCacheData
  static var reloadIgnoringCacheData: NSURLRequestCachePolicy { get }
  case returnCacheDataElseLoad
  case returnCacheDataDontLoad
  case reloadRevalidatingCacheData
}
enum NSURLRequestNetworkServiceType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case networkServiceTypeDefault
  case networkServiceTypeVoIP
  case networkServiceTypeVideo
  case networkServiceTypeBackground
  case networkServiceTypeVoice
}
class NSURLRequest : NSObject, NSSecureCoding, NSCopying, NSMutableCopying {
  convenience init(url URL: NSURL)
  init(url URL: NSURL, cachePolicy cachePolicy: NSURLRequestCachePolicy, timeoutInterval timeoutInterval: NSTimeInterval)
  @NSCopying var url: NSURL? { get }
  var cachePolicy: NSURLRequestCachePolicy { get }
  var timeoutInterval: NSTimeInterval { get }
  @NSCopying var mainDocumentURL: NSURL? { get }
  @available(watchOS 2.0, *)
  var networkServiceType: NSURLRequestNetworkServiceType { get }
  @available(watchOS 2.0, *)
  var allowsCellularAccess: Bool { get }
}
class NSMutableURLRequest : NSURLRequest {
}
extension NSURLRequest {
  var httpMethod: String? { get }
  var allHTTPHeaderFields: [String : String]? { get }
  @discardableResult
  func value(forHTTPHeaderField field: String) -> String?
  @NSCopying var httpBody: NSData? { get }
  var httpBodyStream: NSInputStream? { get }
  var httpShouldHandleCookies: Bool { get }
  @available(watchOS 2.0, *)
  var httpShouldUsePipelining: Bool { get }
}
extension NSMutableURLRequest {
  func setValue(_ value: String?, forHTTPHeaderField field: String)
  func addValue(_ value: String, forHTTPHeaderField field: String)
}
