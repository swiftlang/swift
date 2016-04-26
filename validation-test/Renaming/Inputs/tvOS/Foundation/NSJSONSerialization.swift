
@available(tvOS 5.0, *)
struct NSJSONReadingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var mutableContainers: NSJSONReadingOptions { get }
  static var mutableLeaves: NSJSONReadingOptions { get }
  static var allowFragments: NSJSONReadingOptions { get }
}
@available(tvOS 5.0, *)
struct NSJSONWritingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var prettyPrinted: NSJSONWritingOptions { get }
}
@available(tvOS 5.0, *)
class NSJSONSerialization : NSObject {
  @discardableResult
  class func isValidJSONObject(_ obj: AnyObject) -> Bool
  @discardableResult
  class func data(withJSONObject obj: AnyObject, options opt: NSJSONWritingOptions = []) throws -> NSData
  @discardableResult
  class func jsonObject(with data: NSData, options opt: NSJSONReadingOptions = []) throws -> AnyObject
  @discardableResult
  class func writeJSONObject(_ obj: AnyObject, to stream: NSOutputStream, options opt: NSJSONWritingOptions = [], error error: NSErrorPointer) -> Int
  @discardableResult
  class func jsonObject(with stream: NSInputStream, options opt: NSJSONReadingOptions = []) throws -> AnyObject
}
