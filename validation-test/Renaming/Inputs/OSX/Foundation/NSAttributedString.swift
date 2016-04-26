
@available(OSX 10.0, *)
class NSAttributedString : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  var string: String { get }
  @discardableResult
  func attributes(at location: Int, effectiveRange range: NSRangePointer?) -> [String : AnyObject]
}
extension NSAttributedString {
  var length: Int { get }
  @discardableResult
  func attribute(_ attrName: String, at location: Int, effectiveRange range: NSRangePointer?) -> AnyObject?
  @discardableResult
  func attributedSubstring(from range: NSRange) -> NSAttributedString
  @discardableResult
  func attributes(at location: Int, longestEffectiveRange range: NSRangePointer?, in rangeLimit: NSRange) -> [String : AnyObject]
  @discardableResult
  func attribute(_ attrName: String, at location: Int, longestEffectiveRange range: NSRangePointer?, in rangeLimit: NSRange) -> AnyObject?
  @discardableResult
  func isEqual(to other: NSAttributedString) -> Bool
  init(string str: String)
  init(string str: String, attributes attrs: [String : AnyObject]? = [:])
  init(attributedString attrStr: NSAttributedString)
  @available(OSX 10.6, *)
  func enumerateAttributes(in enumerationRange: NSRange, options opts: NSAttributedStringEnumerationOptions = [], using block: ([String : AnyObject], NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.6, *)
  func enumerateAttribute(_ attrName: String, in enumerationRange: NSRange, options opts: NSAttributedStringEnumerationOptions = [], using block: (AnyObject?, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}
struct NSAttributedStringEnumerationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var reverse: NSAttributedStringEnumerationOptions { get }
  static var longestEffectiveRangeNotRequired: NSAttributedStringEnumerationOptions { get }
}
@available(OSX 10.0, *)
class NSMutableAttributedString : NSAttributedString {
  func replaceCharacters(in range: NSRange, with str: String)
  func setAttributes(_ attrs: [String : AnyObject]?, range range: NSRange)
}
extension NSMutableAttributedString {
  var mutableString: NSMutableString { get }
  func addAttribute(_ name: String, value value: AnyObject, range range: NSRange)
  func addAttributes(_ attrs: [String : AnyObject] = [:], range range: NSRange)
  func removeAttribute(_ name: String, range range: NSRange)
  func replaceCharacters(in range: NSRange, with attrString: NSAttributedString)
  func insert(_ attrString: NSAttributedString, at loc: Int)
  func append(_ attrString: NSAttributedString)
  func deleteCharacters(in range: NSRange)
  func setAttributedString(_ attrString: NSAttributedString)
  func beginEditing()
  func endEditing()
}
