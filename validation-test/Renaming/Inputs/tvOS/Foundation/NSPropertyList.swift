
struct NSPropertyListMutabilityOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var mutableContainers: NSPropertyListMutabilityOptions { get }
  static var mutableContainersAndLeaves: NSPropertyListMutabilityOptions { get }
}
enum NSPropertyListFormat : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case openStepFormat
  case xmlFormat_v1_0
  case binaryFormat_v1_0
}
typealias NSPropertyListReadOptions = NSPropertyListMutabilityOptions
typealias NSPropertyListWriteOptions = Int
class NSPropertyListSerialization : NSObject {
  @discardableResult
  class func propertyList(_ plist: AnyObject, isValidFor format: NSPropertyListFormat) -> Bool
  @available(tvOS 4.0, *)
  @discardableResult
  class func data(fromPropertyList plist: AnyObject, format format: NSPropertyListFormat, options opt: NSPropertyListWriteOptions) throws -> NSData
  @available(tvOS 4.0, *)
  @discardableResult
  class func writePropertyList(_ plist: AnyObject, to stream: NSOutputStream, format format: NSPropertyListFormat, options opt: NSPropertyListWriteOptions, error error: NSErrorPointer) -> Int
  @available(tvOS 4.0, *)
  @discardableResult
  class func propertyList(from data: NSData, options opt: NSPropertyListReadOptions = [], format format: UnsafeMutablePointer<NSPropertyListFormat>?) throws -> AnyObject
  @available(tvOS 4.0, *)
  @discardableResult
  class func propertyList(with stream: NSInputStream, options opt: NSPropertyListReadOptions = [], format format: UnsafeMutablePointer<NSPropertyListFormat>?) throws -> AnyObject
  @available(tvOS, introduced: 2.0, deprecated: 8.0, message: "Use dataWithPropertyList:format:options:error: instead.")
  @discardableResult
  class func dataFromPropertyList(_ plist: AnyObject, format format: NSPropertyListFormat, errorDescription errorString: UnsafeMutablePointer<NSString?>?) -> NSData?
  @available(tvOS, introduced: 2.0, deprecated: 8.0, message: "Use propertyListWithData:options:format:error: instead.")
  @discardableResult
  class func propertyListFromData(_ data: NSData, mutabilityOption opt: NSPropertyListMutabilityOptions = [], format format: UnsafeMutablePointer<NSPropertyListFormat>?, errorDescription errorString: UnsafeMutablePointer<NSString?>?) -> AnyObject?
}
