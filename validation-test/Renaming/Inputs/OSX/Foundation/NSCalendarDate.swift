
extension NSDate {
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  @discardableResult
  class func date(withNaturalLanguageString string: String, locale locale: AnyObject?) -> AnyObject?
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  @discardableResult
  class func date(withNaturalLanguageString string: String) -> AnyObject?
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  @discardableResult
  class func date(with aString: String) -> AnyObject
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  @discardableResult
  func date(withCalendarFormat format: String?, timeZone aTimeZone: NSTimeZone?) -> NSCalendarDate
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  @discardableResult
  func description(withCalendarFormat format: String?, timeZone aTimeZone: NSTimeZone?, locale locale: AnyObject?) -> String?
  @available(OSX, introduced: 10.4, deprecated: 10.10)
  convenience init?(string description: String)
}
