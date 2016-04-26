
class NSScanner : NSObject, NSCopying {
  var string: String { get }
  var scanLocation: Int
  @NSCopying var charactersToBeSkipped: NSCharacterSet?
  var caseSensitive: Bool
  var locale: AnyObject?
  init(string string: String)
}
extension NSScanner {
  @discardableResult
  func scanInt32(_ result: UnsafeMutablePointer<Int32>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func scanInt(_ result: UnsafeMutablePointer<Int>?) -> Bool
  @discardableResult
  func scanInt64(_ result: UnsafeMutablePointer<Int64>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func scanUnsignedLongLong(_ result: UnsafeMutablePointer<UInt64>?) -> Bool
  @discardableResult
  func scanFloat(_ result: UnsafeMutablePointer<Float>?) -> Bool
  @discardableResult
  func scanDouble(_ result: UnsafeMutablePointer<Double>?) -> Bool
  @discardableResult
  func scanHexInt32(_ result: UnsafeMutablePointer<UInt32>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func scanHexInt64(_ result: UnsafeMutablePointer<UInt64>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func scanHexFloat(_ result: UnsafeMutablePointer<Float>?) -> Bool
  @available(watchOS 2.0, *)
  @discardableResult
  func scanHexDouble(_ result: UnsafeMutablePointer<Double>?) -> Bool
  @discardableResult
  func scanString(_ string: String, into result: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  @discardableResult
  func scanCharacters(from set: NSCharacterSet, into result: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  @discardableResult
  func scanUpTo(_ string: String, into result: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  @discardableResult
  func scanUpToCharacters(from set: NSCharacterSet, into result: AutoreleasingUnsafeMutablePointer<NSString?>?) -> Bool
  var isAtEnd: Bool { get }
  @discardableResult
  class func localizedScanner(with string: String) -> AnyObject
}
