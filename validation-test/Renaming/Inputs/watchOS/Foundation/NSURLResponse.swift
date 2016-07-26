
class NSURLResponse : NSObject, NSSecureCoding, NSCopying {
  init(url URL: NSURL, mimeType MIMEType: String?, expectedContentLength length: Int, textEncodingName name: String?)
  @NSCopying var url: NSURL? { get }
  var mimeType: String? { get }
  var expectedContentLength: Int64 { get }
  var textEncodingName: String? { get }
  var suggestedFilename: String? { get }
}
class NSHTTPURLResponse : NSURLResponse {
  @available(watchOS 2.0, *)
  init?(url url: NSURL, statusCode statusCode: Int, httpVersion HTTPVersion: String?, headerFields headerFields: [String : String]?)
  var statusCode: Int { get }
  var allHeaderFields: [NSObject : AnyObject] { get }
  @discardableResult
  class func localizedString(forStatusCode statusCode: Int) -> String
}
