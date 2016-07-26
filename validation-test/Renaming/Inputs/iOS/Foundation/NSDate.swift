
@available(iOS 4.0, *)
let NSSystemClockDidChangeNotification: String
typealias NSTimeInterval = Double
var NSTimeIntervalSince1970: Double { get }
class NSDate : NSObject, NSCopying, NSSecureCoding {
  var timeIntervalSinceReferenceDate: NSTimeInterval { get }
  init(timeIntervalSinceReferenceDate ti: NSTimeInterval)
}

extension NSDate : CustomPlaygroundQuickLookable {
  var summary: String { get }
}
extension NSDate {
  @discardableResult
  func timeIntervalSince(_ anotherDate: NSDate) -> NSTimeInterval
  var timeIntervalSinceNow: NSTimeInterval { get }
  var timeIntervalSince1970: NSTimeInterval { get }
  @available(iOS 2.0, *)
  @discardableResult
  func addingTimeInterval(_ ti: NSTimeInterval) -> Self
  @discardableResult
  func earlierDate(_ anotherDate: NSDate) -> NSDate
  @discardableResult
  func laterDate(_ anotherDate: NSDate) -> NSDate
  @discardableResult
  func compare(_ other: NSDate) -> NSComparisonResult
  @discardableResult
  func isEqual(to otherDate: NSDate) -> Bool
  @discardableResult
  func description(with locale: AnyObject?) -> String
  @discardableResult
  class func timeIntervalSinceReferenceDate() -> NSTimeInterval
}
extension NSDate {
  @discardableResult
  class func distantFuture() -> NSDate
  @discardableResult
  class func distantPast() -> NSDate
  convenience init(timeIntervalSinceNow secs: NSTimeInterval)
  convenience init(timeIntervalSince1970 secs: NSTimeInterval)
  convenience init(timeInterval secsToBeAdded: NSTimeInterval, since date: NSDate)
}
