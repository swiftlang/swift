
enum SLRequestMethod : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case GET
  case POST
  case DELETE
  case PUT
}
typealias SLRequestHandler = (NSData!, NSHTTPURLResponse!, NSError!) -> Void
@available(iOS 6.0, *)
class SLRequest : NSObject {
  /*not inherited*/ init!(forServiceType serviceType: String!, requestMethod requestMethod: SLRequestMethod, url url: NSURL!, parameters parameters: [NSObject : AnyObject]!)
  var requestMethod: SLRequestMethod { get }
  var url: NSURL! { get }
  var parameters: [NSObject : AnyObject]! { get }
  func addMultipartData(_ data: NSData!, withName name: String!, type type: String!, filename filename: String!)
  @discardableResult
  func preparedURLRequest() -> NSURLRequest!
  func perform(handler handler: SLRequestHandler!)
}
