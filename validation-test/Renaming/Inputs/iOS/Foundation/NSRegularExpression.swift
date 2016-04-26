
struct NSRegularExpressionOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var caseInsensitive: NSRegularExpressionOptions { get }
  static var allowCommentsAndWhitespace: NSRegularExpressionOptions { get }
  static var ignoreMetacharacters: NSRegularExpressionOptions { get }
  static var dotMatchesLineSeparators: NSRegularExpressionOptions { get }
  static var anchorsMatchLines: NSRegularExpressionOptions { get }
  static var useUnixLineSeparators: NSRegularExpressionOptions { get }
  static var useUnicodeWordBoundaries: NSRegularExpressionOptions { get }
}
@available(iOS 4.0, *)
class NSRegularExpression : NSObject, NSCopying, NSSecureCoding {
  init(pattern pattern: String, options options: NSRegularExpressionOptions = []) throws
  var pattern: String { get }
  var options: NSRegularExpressionOptions { get }
  var numberOfCaptureGroups: Int { get }
  @discardableResult
  class func escapedPattern(for string: String) -> String
}
struct NSMatchingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var reportProgress: NSMatchingOptions { get }
  static var reportCompletion: NSMatchingOptions { get }
  static var anchored: NSMatchingOptions { get }
  static var withTransparentBounds: NSMatchingOptions { get }
  static var withoutAnchoringBounds: NSMatchingOptions { get }
}
struct NSMatchingFlags : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var progress: NSMatchingFlags { get }
  static var completed: NSMatchingFlags { get }
  static var hitEnd: NSMatchingFlags { get }
  static var requiredEnd: NSMatchingFlags { get }
  static var internalError: NSMatchingFlags { get }
}
extension NSRegularExpression {
  func enumerateMatches(in string: String, options options: NSMatchingOptions = [], range range: NSRange, using block: (NSTextCheckingResult?, NSMatchingFlags, UnsafeMutablePointer<ObjCBool>) -> Void)
  @discardableResult
  func matches(in string: String, options options: NSMatchingOptions = [], range range: NSRange) -> [NSTextCheckingResult]
  @discardableResult
  func numberOfMatches(in string: String, options options: NSMatchingOptions = [], range range: NSRange) -> Int
  @discardableResult
  func firstMatch(in string: String, options options: NSMatchingOptions = [], range range: NSRange) -> NSTextCheckingResult?
  @discardableResult
  func rangeOfFirstMatch(in string: String, options options: NSMatchingOptions = [], range range: NSRange) -> NSRange
}
extension NSRegularExpression {
  @discardableResult
  func stringByReplacingMatches(in string: String, options options: NSMatchingOptions = [], range range: NSRange, withTemplate templ: String) -> String
  @discardableResult
  func replaceMatches(in string: NSMutableString, options options: NSMatchingOptions = [], range range: NSRange, withTemplate templ: String) -> Int
  @discardableResult
  func replacementString(for result: NSTextCheckingResult, in string: String, offset offset: Int, template templ: String) -> String
  @discardableResult
  class func escapedTemplate(for string: String) -> String
}
@available(iOS 4.0, *)
class NSDataDetector : NSRegularExpression {
  init(types checkingTypes: NSTextCheckingTypes) throws
  var checkingTypes: NSTextCheckingTypes { get }
}
