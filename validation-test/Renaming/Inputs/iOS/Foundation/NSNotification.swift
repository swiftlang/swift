
class NSNotification : NSObject, NSCopying, NSCoding {
  var name: String { get }
  var object: AnyObject? { get }
  var userInfo: [NSObject : AnyObject]? { get }
  @available(iOS 4.0, *)
  init(name name: String, object object: AnyObject?, userInfo userInfo: [NSObject : AnyObject]? = [:])
}
extension NSNotification {
  convenience init(name aName: String, object anObject: AnyObject?)
}
class NSNotificationCenter : NSObject {
  @discardableResult
  class func defaultCenter() -> NSNotificationCenter
  func addObserver(_ observer: AnyObject, selector aSelector: Selector, name aName: String?, object anObject: AnyObject?)
  func post(_ notification: NSNotification)
  func post(name aName: String, object anObject: AnyObject?)
  func post(name aName: String, object anObject: AnyObject?, userInfo aUserInfo: [NSObject : AnyObject]? = [:])
  func removeObserver(_ observer: AnyObject)
  func removeObserver(_ observer: AnyObject, name aName: String?, object anObject: AnyObject?)
  @available(iOS 4.0, *)
  @discardableResult
  func addObserver(forName name: String?, object obj: AnyObject?, queue queue: NSOperationQueue?, using block: (NSNotification) -> Void) -> NSObjectProtocol
}
